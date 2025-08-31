#include "ILVerify.hpp"

#include <regex>
#include <sstream>
#include <unordered_map>
#include <unordered_set>

#include "common/IL.hpp"

namespace bitty::IL {
namespace {

using SymKind = int;
static constexpr SymKind VAR = 1;
static constexpr SymKind FUN = 2;

struct Scope {
  std::unordered_map<std::string, SymKind> table;
};

struct VCtx {
  bool strict = false;
  std::vector<Scope> scopes;
  std::vector<std::string> errors;
  std::unordered_set<std::string> seenTemps;
  std::regex tempRe{R"(^__t[0-9]+$)"};

  void push() {
    scopes.emplace_back();
  }
  void pop() {
    scopes.pop_back();
  }

  void error(std::string msg) {
    errors.push_back(std::move(msg));
  }

  void addVar(const std::string& n) {
    if (scopes.empty()) {
      push();
    }
    scopes.back().table[n] = VAR;
    if (strict && std::regex_match(n, tempRe)) {
      if (!seenTemps.insert(n).second) {
        error("temp redeclared: " + n);
      }
    }
  }
  void addFun(const std::string& n) {
    if (scopes.empty()) {
      push();
    }
    scopes.back().table[n] = FUN;
  }

  SymKind lookup(const std::string& n) const {
    for (int i = (int)scopes.size() - 1; i >= 0; --i) {
      auto it = scopes[i].table.find(n);
      if (it != scopes[i].table.end()) {
        return it->second;
      }
    }
    return 0;
  }

  bool isSimpleExpr(const IL::Expression& e) {
    return std::visit(
        [&](auto const& node) -> bool {
          using T = std::decay_t<decltype(node)>;
          if constexpr (std::is_same_v<T, IL::LiteralExpr>) {
            return true;
          }
          if constexpr (std::is_same_v<T, IL::VariableExpr>) {
            return true;
          }
          if constexpr (std::is_same_v<T, IL::UnaryExpr>) {
            return isSimpleExpr(*node.right);
          }
          if constexpr (std::is_same_v<T, IL::BinaryExpr>) {
            return isSimpleExpr(*node.left) && isSimpleExpr(*node.right);
          }
          return false;
        },
        e.node);
  }

  void checkExpr(const IL::Expression& e) {
    if (!isSimpleExpr(e)) {
      error("non-simple expression found in IL");
    }
  }

  void checkBlock(const IL::BlockStmt& b);

  void checkStmt(const IL::Statement& s) {
    std::visit(
        [&](auto const& st) {
          using T = std::decay_t<decltype(st)>;

          if constexpr (std::is_same_v<T, IL::PrintStmt>) {
            checkExpr(*st.expression);

          } else if constexpr (std::is_same_v<T, IL::BlockStmt>) {
            checkBlock(st);

          } else if constexpr (std::is_same_v<T, IL::VarDeclStmt>) {
            if (strict && lookup(st.name)) {
              error("redeclaration: " + st.name);
            }
            addVar(st.name);

          } else if constexpr (std::is_same_v<T, IL::AssignmentStmt>) {
            if (strict && !lookup(st.targetName)) {
              error("assign to undeclared var: " + st.targetName);
            }
            checkExpr(*st.value);

          } else if constexpr (std::is_same_v<T, IL::IfStmt>) {
            checkExpr(*st.condition);

            if (!st.thenBranch) {
              error("thenBranch is null");
            } else if (!std::holds_alternative<IL::BlockStmt>(
                           st.thenBranch->node)) {
              error("thenBranch must be BlockStmt");
            } else {
              checkBlock(std::get<IL::BlockStmt>(st.thenBranch->node));
            }

            if (st.elseBranch) {
              if (!std::holds_alternative<IL::BlockStmt>(st.elseBranch->node)) {
                error("elseBranch must be BlockStmt");
              } else {
                checkBlock(std::get<IL::BlockStmt>(st.elseBranch->node));
              }
            }

          } else if constexpr (std::is_same_v<T, IL::WhileStmt>) {
            checkExpr(*st.condition);

            if (!st.body) {
              error("while body is null");
            } else if (!std::holds_alternative<IL::BlockStmt>(st.body->node)) {
              error("while body must be BlockStmt");
            } else {
              checkBlock(std::get<IL::BlockStmt>(st.body->node));
            }

          } else if constexpr (std::is_same_v<T, IL::ReturnStmt>) {
            if (st.value) {
              checkExpr(*st.value);
            }

          } else if constexpr (std::is_same_v<T, IL::CallStmt>) {
            for (auto const& a : st.arguments) {
              checkExpr(*a);
            }
            if (strict) {
              auto k = lookup(st.calleeName);
              if (k != FUN) {
                error("call to non-function or undeclared: " + st.calleeName);
              }
              if (st.targetName && !lookup(*st.targetName)) {
                error("call result assigned to undeclared: " + *st.targetName);
              }
            }

          } else if constexpr (std::is_same_v<T, IL::FuncDeclStmt>) {
            if (strict && lookup(st.name)) {
              error("function redeclaration: " + st.name);
            }
            addFun(st.name);

            push();
            for (auto const& p : st.params) {
              addVar(p);
            }

            if (!st.body) {
              error("function body is null");
            } else {
              checkBlock(*st.body);
            }
            pop();

          } else {
            static_assert(!sizeof(T*), "unhandled IL stmt");
          }
        },
        s.node);
  }
};

void VCtx::checkBlock(const IL::BlockStmt& b) {
  push();
  for (auto const& sp : b.statements) {
    if (!sp) {
      error("null statement inside IL::BlockStmt");
      continue;
    }
    checkStmt(*sp);
  }
  pop();
}
}  // namespace

VerifyResult verify(const Program& p, bool strictSymbols) {
  VCtx v;
  v.strict = strictSymbols;

  v.push();
  for (auto const& st : p.statements) {
    if (!st) {
      v.error("null top-level statement");
      continue;
    }
    v.checkStmt(*st);
  }
  v.pop();

  VerifyResult r;
  r.ok = v.errors.empty();
  r.errors = std::move(v.errors);
  return r;
}

}  // namespace bitty::IL
