#pragma once

#include <filesystem>
#include <fstream>
#include <regex>
#include <sstream>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <vector>

#include "BittyLexer.h"
#include "BittyParser.h"
#include "antlr4-runtime.h"
#include "core/AST.hpp"
#include "core/ASTBuilder.hpp"
#include "core/IL.hpp"

using namespace bitty;

class ThrowingErrorListener : public antlr4::BaseErrorListener {
 public:
  void syntaxError(antlr4::Recognizer*, antlr4::Token*, size_t line, size_t col,
                   const std::string& msg, std::exception_ptr) override {
    std::ostringstream oss;
    oss << "syntax error at " << line << ":" << col << " - " << msg;
    throw std::runtime_error(oss.str());
  }
};

static AST::Program parseToAST(const std::string& src) {
  antlr4::ANTLRInputStream input(src);
  bitty::parser::BittyLexer lexer(&input);
  ThrowingErrorListener err;
  lexer.removeErrorListeners();
  lexer.addErrorListener(&err);

  antlr4::CommonTokenStream tokens(&lexer);

  bitty::parser::BittyParser parser(&tokens);
  parser.removeErrorListeners();
  parser.addErrorListener(&err);

  auto* programCtx = parser.program();

  ASTBuilder ab;
  return ab.build(programCtx);
}

inline std::string readFileText(const std::string& path) {
  std::ifstream ifs(path, std::ios::binary);
  if (!ifs) {
    throw std::runtime_error("failed to open: " + path);
  }
  std::ostringstream oss;
  oss << ifs.rdbuf();
  return oss.str();
}

inline void writeFileText(const std::string& path, const std::string& text) {
  std::filesystem::create_directories(
      std::filesystem::path(path).parent_path());
  std::ofstream ofs(path, std::ios::binary | std::ios::trunc);
  if (!ofs) {
    throw std::runtime_error("failed to write: " + path);
  }
  ofs << text;
}

inline std::vector<std::string> collect_case_files(
    const std::string& dir_glob_root) {
  namespace fs = std::filesystem;
  std::vector<std::string> out;
  fs::path root(dir_glob_root);
  if (!fs::exists(root)) {
    return out;
  }
  for (auto const& e : fs::directory_iterator(root)) {
    if (!e.is_regular_file()) {
      continue;
    }
    auto p = e.path();
    if (p.extension() == ".bitty") {
      out.push_back(p.string());
    }
  }
  std::sort(out.begin(), out.end());
  return out;
}

inline bool shouldUpdateSnapshots() {
  const char* v = std::getenv("BITTY_UPDATE_SNAPSHOTS");
  return v && *v && std::string(v) != "0";
}

template <class... Ts>
struct overloaded : Ts... {
  using Ts::operator()...;
};
template <class... Ts>
overloaded(Ts...) -> overloaded<Ts...>;

static int countAstCalls(const AST::Program& p) {
  int n = 0;
  std::function<void(const AST::Expression&)> walkE = [&](auto const& e) {
    std::visit(overloaded{
                   [&](const AST::LiteralExpr&) {},
                   [&](const AST::VariableExpr&) {},
                   [&](const AST::UnaryExpr& u) { walkE(*u.right); },
                   [&](const AST::BinaryExpr& b) {
                     walkE(*b.left);
                     walkE(*b.right);
                   },
                   [&](const AST::AssignmentExpr& a) {
                     walkE(*a.target);
                     walkE(*a.value);
                   },
                   [&](const AST::CallExpr& c) {
                     ++n;
                     walkE(*c.callee);
                     for (auto const& a : c.arguments) {
                       walkE(*a);
                     }
                   },
               },
               e.node);
  };
  std::function<void(const AST::Statement&)> walkS = [&](auto const& s) {
    std::visit(
        overloaded{
            [&](const AST::ExpressionStmt& st) { walkE(*st.expression); },
            [&](const AST::PrintStmt& st) { walkE(*st.expression); },
            [&](const AST::BlockStmt& b) {
              for (auto const& sp : b.statements) {
                walkS(*sp);
              }
            },
            [&](const AST::VarDeclStmt& st) {
              if (st.initializer) {
                walkE(*st.initializer);
              }
            },
            [&](const AST::IfStmt& st) {
              walkE(*st.condition);
              walkS(*st.thenBranch);
              if (st.elseBranch) {
                walkS(*st.elseBranch);
              }
            },
            [&](const AST::WhileStmt& st) {
              walkE(*st.condition);
              walkS(*st.body);
            },
            [&](const AST::ReturnStmt& st) {
              if (st.value) {
                walkE(*st.value);
              }
            },
            [&](const AST::FuncDeclStmt& fd) {
              for (auto const& sp : fd.body->statements) {
                walkS(*sp);
              }
            },
        },
        s.node);
  };
  for (auto const& st : p.statements) {
    walkS(*st);
  }
  return n;
}

static int countAstExprStmtCalls(const AST::Program& p) {
  int n = 0;
  std::function<void(const AST::Statement&)> walkS = [&](auto const& s) {
    std::visit(
        overloaded{
            [&](const AST::ExpressionStmt& st) {
              if (std::holds_alternative<AST::CallExpr>(st.expression->node)) {
                ++n;
              }
            },
            [&](const AST::PrintStmt&) {},
            [&](const AST::BlockStmt& b) {
              for (auto const& sp : b.statements) {
                walkS(*sp);
              }
            },
            [&](const AST::VarDeclStmt&) {},
            [&](const AST::IfStmt& st) {
              walkS(*st.thenBranch);
              if (st.elseBranch) {
                walkS(*st.elseBranch);
              }
            },
            [&](const AST::WhileStmt& st) { walkS(*st.body); },
            [&](const AST::ReturnStmt&) {},
            [&](const AST::FuncDeclStmt& fd) {
              for (auto const& sp : fd.body->statements) {
                walkS(*sp);
              }
            },
        },
        s.node);
  };
  for (auto const& st : p.statements) {
    walkS(*st);
  }
  return n;
}

static int countIlCallStmts(const IL::Program& p) {
  int n = 0;
  std::function<void(const IL::Statement&)> walkS = [&](auto const& s) {
    std::visit(overloaded{
                   [&](const IL::CallStmt&) { ++n; },
                   [&](const IL::PrintStmt&) {},
                   [&](const IL::BlockStmt& b) {
                     for (auto const& sp : b.statements) {
                       walkS(*sp);
                     }
                   },
                   [&](const IL::VarDeclStmt&) {},
                   [&](const IL::AssignmentStmt&) {},
                   [&](const IL::IfStmt& st) {
                     walkS(*st.thenBranch);
                     if (st.elseBranch) {
                       walkS(*st.elseBranch);
                     }
                   },
                   [&](const IL::WhileStmt& st) { walkS(*st.body); },
                   [&](const IL::ReturnStmt&) {},
                   [&](const IL::FuncDeclStmt& fd) {
                     for (auto const& sp : fd.body->statements) {
                       walkS(*sp);
                     }
                   },
               },
               s.node);
  };
  for (auto const& st : p.statements) {
    walkS(*st);
  }
  return n;
}
static int countIlVoidCalls(const IL::Program& p) {
  int n = 0;
  std::function<void(const IL::Statement&)> walkS = [&](auto const& s) {
    std::visit(overloaded{
                   [&](const IL::CallStmt& cs) {
                     if (!cs.targetName) {
                       ++n;
                     }
                   },
                   [&](const IL::PrintStmt&) {},
                   [&](const IL::BlockStmt& b) {
                     for (auto const& sp : b.statements) {
                       walkS(*sp);
                     }
                   },
                   [&](const IL::VarDeclStmt&) {},
                   [&](const IL::AssignmentStmt&) {},
                   [&](const IL::IfStmt& st) {
                     walkS(*st.thenBranch);
                     if (st.elseBranch) {
                       walkS(*st.elseBranch);
                     }
                   },
                   [&](const IL::WhileStmt& st) { walkS(*st.body); },
                   [&](const IL::ReturnStmt&) {},
                   [&](const IL::FuncDeclStmt& fd) {
                     for (auto const& sp : fd.body->statements) {
                       walkS(*sp);
                     }
                   },
               },
               s.node);
  };
  for (auto const& st : p.statements) {
    walkS(*st);
  }
  return n;
}

inline std::string normalizeWhitespace(std::string s) {
  std::string out;
  out.reserve(s.size());
  std::string line;
  for (size_t i = 0; i < s.size(); ++i) {
    char c = s[i];
    if (c == '\r') {
      continue;
    }
    if (c == '\n') {
      // rtrim
      while (!line.empty() && (line.back() == ' ' || line.back() == '\t')) {
        line.pop_back();
      }
      out += line;
      out += '\n';
      line.clear();
    } else {
      line += c;
    }
  }
  if (!line.empty()) {
    while (!line.empty() && (line.back() == ' ' || line.back() == '\t')) {
      line.pop_back();
    }
    out += line;
  }
  return out;
}

inline std::string canonicalizeTemps(const std::string& s) {
  std::regex re(R"(__t([0-9]+))");
  std::unordered_map<std::string, std::string> map;
  int nextId = 0;
  std::string out;
  out.reserve(s.size());
  std::sregex_iterator it(s.begin(), s.end(), re), end;
  size_t pos = 0;

  auto subst = [&](const std::string& name) -> std::string {
    auto it = map.find(name);
    if (it != map.end()) {
      return it->second;
    }
    std::string canon = "T" + std::to_string(nextId++);
    map.emplace(name, canon);
    return canon;
  };

  for (; it != end; ++it) {
    auto m = *it;
    size_t b = m.position();
    size_t e = b + m.length();
    out.append(s, pos, b - pos);
    std::string orig = m.str();  // "__tNN"
    out += subst(orig);
    pos = e;
  }
  out.append(s, pos, std::string::npos);

  return out;
}

inline std::string normalizeILText(std::string s) {
  return canonicalizeTemps(normalizeWhitespace(std::move(s)));
}
