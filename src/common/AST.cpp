#include "common/AST.hpp"

#include <iomanip>
#include <limits>
#include <ostream>
#include <sstream>
#include <string>
#include <variant>

namespace bitty::AST {
namespace {

constexpr int IND = 2;
inline void indent(std::ostream& os, int level) {
  for (int i = 0; i < level * IND; ++i) {
    os.put(' ');
  }
}

enum Prec : int {
  PREC_ASSIGN = 10,    // =
  PREC_LOGOR = 15,     // ||
  PREC_LOGAND = 17,    // &&
  PREC_EQUALITY = 20,  // == !=
  PREC_COMPARE = 30,   // < > <= >=
  PREC_ADD = 40,       // + -
  PREC_MUL = 50,       // * /
  PREC_UNARY = 60,     // +x -x
  PREC_CALL = 70,      // f(...)
  PREC_PRIMARY = 80    // literal / id / (...)
};

inline int precBin(const BinaryOp& op) {
  switch (op) {
    case BinaryOp::ADD:
    case BinaryOp::SUB:
      return PREC_ADD;
    case BinaryOp::MUL:
    case BinaryOp::DIV:
      // case BinaryOp::MOD:
      return PREC_MUL;
    case BinaryOp::EQ:
    case BinaryOp::NEQ:
      return PREC_EQUALITY;
    case BinaryOp::LT:
    case BinaryOp::LTE:
    case BinaryOp::GT:
    case BinaryOp::GTE:
      return PREC_COMPARE;
    case BinaryOp::AND:
      return PREC_LOGAND;
    case BinaryOp::OR:
      return PREC_LOGOR;
    default:
      return PREC_PRIMARY;
  }
}

inline bool nonAssocRight(const BinaryOp& op) {
  return (op == BinaryOp::SUB || op == BinaryOp::DIV);
}

inline int precOf(const AST::ExpressionNode& n) {
  return std::visit(
      [&](auto const& node) -> int {
        using T = std::decay_t<decltype(node)>;
        if constexpr (std::is_same_v<T, AST::LiteralExpr> ||
                      std::is_same_v<T, AST::VariableExpr>) {
          return PREC_PRIMARY;
        } else if constexpr (std::is_same_v<T, AST::UnaryExpr>) {
          return PREC_UNARY;
        } else if constexpr (std::is_same_v<T, AST::BinaryExpr>) {
          return precBin(node.op);
        } else if constexpr (std::is_same_v<T, AST::AssignmentExpr>) {
          return PREC_ASSIGN;
        } else if constexpr (std::is_same_v<T, AST::CallExpr>) {
          return PREC_CALL;
        }
        return PREC_PRIMARY;
      },
      n);
}

bool isBlockStmt(const AST::Statement& s) {
  return std::holds_alternative<AST::BlockStmt>(s.node);
}

bool isIfWithoutElse(const AST::Statement& s) {
  if (!std::holds_alternative<AST::IfStmt>(s.node)) {
    return false;
  }
  const auto& is = std::get<AST::IfStmt>(s.node);
  return !is.elseBranch;
}

std::string formatDoubleLiteral(double x) {
  std::ostringstream oss;
  oss << std::setprecision(std::numeric_limits<double>::max_digits10);
  oss << x;
  std::string s = oss.str();
  if (s.find('e') != std::string::npos || s.find('E') != std::string::npos) {
    std::ostringstream oss2;
    oss2.setf(std::ios::fixed);
    oss2 << std::setprecision(16) << x;
    s = oss2.str();
  }
  if (s.find('.') == std::string::npos) {
    s += ".0";
  } else {
    while (!s.empty() && s.back() == '0') {
      s.pop_back();
    }
    if (!s.empty() && s.back() == '.') {
      s += '0';
    }
  }
  return s;
}

void emitLiteral(const LiteralValue& v, std::ostream& os) {
  std::visit(
      [&](auto const& val) {
        using T = std::decay_t<decltype(val)>;
        if constexpr (std::is_same_v<T, std::monostate>) {
          os << "nil";
        } else if constexpr (std::is_same_v<T, bool>) {
          os << (val ? "true" : "false");
        } else if constexpr (std::is_same_v<T, int64_t>) {
          os << val;
        } else if constexpr (std::is_same_v<T, double>) {
          os << formatDoubleLiteral(val);
        } else if constexpr (std::is_same_v<T, std::string>) {
          os << "\"";
          for (char c : val) {
            switch (c) {
              case '\\':
                os << "\\\\";
                break;
              case '\"':
                os << "\\\"";
                break;
              case '\n':
                os << "\\n";
                break;
              case '\t':
                os << "\\t";
                break;
              case '\r':
                os << "\\r";
                break;
              default:
                os << c;
                break;
            }
          }
          os << "\"";
        } else {
          static_assert(!sizeof(T*), "Unhandled literal type");
        }
      },
      v);
}

void emitExpr(const AST::Expression& e, std::ostream& os);

void emitExprNode(const AST::ExpressionNode& n, std::ostream& os) {
  std::visit(
      [&](auto const& node) {
        using T = std::decay_t<decltype(node)>;

        if constexpr (std::is_same_v<T, AST::LiteralExpr>) {
          emitLiteral(node.value, os);
        } else if constexpr (std::is_same_v<T, AST::VariableExpr>) {
          os << node.name;
        } else if constexpr (std::is_same_v<T, AST::UnaryExpr>) {
          os << toString(node.op);
          int rp = precOf(node.right->node);
          bool par = (rp < PREC_UNARY);
          if (par) {
            os << '(';
            emitExpr(*node.right, os);
            os << ')';
          } else {
            emitExpr(*node.right, os);
          }
        } else if constexpr (std::is_same_v<T, AST::BinaryExpr>) {
          int my = precBin(node.op);
          int lp = precOf(node.left->node);
          int rp = precOf(node.right->node);
          bool pl = (lp < my);
          bool pr =
              (rp < my) || (rp == my && nonAssocRight(node.op));  // a - (b - c)

          if (pl) {
            os << '(';
            emitExpr(*node.left, os);
            os << ')';
          } else {
            emitExpr(*node.left, os);
          }

          os << ' ' << toString(node.op) << ' ';

          if (pr) {
            os << '(';
            emitExpr(*node.right, os);
            os << ')';
          } else {
            emitExpr(*node.right, os);
          }
        } else if constexpr (std::is_same_v<T, AST::AssignmentExpr>) {
          int lp = precOf(node.target->node);
          int rp = precOf(node.value->node);
          bool pl = (lp < PREC_ASSIGN);
          bool pr = (rp <= PREC_ASSIGN);

          if (pl) {
            os << '(';
            emitExpr(*node.target, os);
            os << ')';
          } else {
            emitExpr(*node.target, os);
          }

          os << " = ";

          if (pr) {
            os << '(';
            emitExpr(*node.value, os);
            os << ')';
          } else {
            emitExpr(*node.value, os);
          }
        } else if constexpr (std::is_same_v<T, AST::CallExpr>) {
          bool par = (precOf(node.callee->node) < PREC_CALL);
          if (par) {
            os << '(';
            emitExpr(*node.callee, os);
            os << ')';
          } else {
            emitExpr(*node.callee, os);
          }

          os << '(';
          for (size_t i = 0; i < node.arguments.size(); ++i) {
            emitExpr(*node.arguments[i], os);
            if (i + 1 < node.arguments.size()) {
              os << ", ";
            }
          }
          os << ')';
        }
      },
      n);
}

void emitExpr(const AST::Expression& e, std::ostream& os) {
  emitExprNode(e.node, os);
}

void emitStmt(const AST::Statement& s, std::ostream& os, int lvl);

void emitBlock(const AST::BlockStmt& b, std::ostream& os, int lvl) {
  os << "{\n";
  for (auto const& sp : b.statements) {
    emitStmt(*sp, os, lvl + 1);
  }
  indent(os, lvl);
  os << "}";
}

void emitStmtNode(const AST::StatementNode& n, std::ostream& os, int lvl) {
  std::visit(
      [&](auto const& node) {
        using T = std::decay_t<decltype(node)>;

        if constexpr (std::is_same_v<T, AST::ExpressionStmt>) {
          indent(os, lvl);
          emitExpr(*node.expression, os);
          os << ";\n";
        } else if constexpr (std::is_same_v<T, AST::PrintStmt>) {
          indent(os, lvl);
          os << "print ";
          emitExpr(*node.expression, os);
          os << ";\n";
        } else if constexpr (std::is_same_v<T, AST::BlockStmt>) {
          indent(os, lvl);
          emitBlock(node, os, lvl);
          os << "\n";
        } else if constexpr (std::is_same_v<T, AST::VarDeclStmt>) {
          indent(os, lvl);
          os << "let " << node.name;
          if (node.initializer) {
            os << " = ";
            emitExpr(*node.initializer, os);
          }
          os << ";\n";
        } else if constexpr (std::is_same_v<T, AST::IfStmt>) {
          // if (cond) <then> [else <else>]
          indent(os, lvl);
          os << "if (";
          emitExpr(*node.condition, os);
          os << ") ";

          const bool hasElse = static_cast<bool>(node.elseBranch);
          const bool thenIsBlock = isBlockStmt(*node.thenBranch);
          const bool needWrapThen =
              hasElse && isIfWithoutElse(*node.thenBranch);

          if (thenIsBlock) {
            emitBlock(std::get<AST::BlockStmt>(node.thenBranch->node), os, lvl);
          } else if (needWrapThen) {
            os << "{\n";
            emitStmt(*node.thenBranch, os, lvl + 1);
            indent(os, lvl);
            os << "}";
          } else {
            os << "\n";
            emitStmt(*node.thenBranch, os, lvl + 1);
          }

          if (hasElse) {
            if (!thenIsBlock && !needWrapThen) {
              indent(os, lvl);
              os << "else ";
            } else {
              os << " else ";
            }

            if (isBlockStmt(*node.elseBranch)) {
              emitBlock(std::get<AST::BlockStmt>(node.elseBranch->node), os,
                        lvl);
              os << "\n";
            } else if (std::holds_alternative<AST::IfStmt>(
                           node.elseBranch->node)) {
              emitStmt(*node.elseBranch, os, lvl);
            } else {
              os << "\n";
              emitStmt(*node.elseBranch, os, lvl + 1);
            }
          } else {
            if (thenIsBlock || needWrapThen) {
              os << "\n";
            }
          }
        } else if constexpr (std::is_same_v<T, AST::WhileStmt>) {
          // while (cond) <body>
          indent(os, lvl);
          os << "while (";
          emitExpr(*node.condition, os);
          os << ") ";

          if (isBlockStmt(*node.body)) {
            emitBlock(std::get<AST::BlockStmt>(node.body->node), os, lvl);
            os << "\n";
          } else {
            os << "\n";
            emitStmt(*node.body, os, lvl + 1);
          }
        } else if constexpr (std::is_same_v<T, AST::ReturnStmt>) {
          indent(os, lvl);
          os << "return";
          if (node.value) {
            os << ' ';
            emitExpr(*node.value, os);
          }
          os << ";\n";
        } else if constexpr (std::is_same_v<T, AST::FuncDeclStmt>) {
          // fn name(a,b,c) { ... }
          indent(os, lvl);
          os << "fn " << node.name << "(";
          for (size_t i = 0; i < node.params.size(); ++i) {
            if (i) {
              os << ", ";
            }
            os << node.params[i];
          }
          os << ") ";
          emitBlock(*node.body, os, lvl);
          os << "\n";
        }
      },
      n);
}

void emitStmt(const AST::Statement& s, std::ostream& os, int lvl) {
  emitStmtNode(s.node, os, lvl);
}
}  // namespace

void format(const Program& program, std::ostream& os,
            int indentLevel /*unused*/) {
  (void)indentLevel;
  for (auto const& st : program.statements) {
    emitStmt(*st, os, 0);
  }
}

std::string toSource(const Program& program) {
  std::ostringstream oss;
  format(program, oss, 0);
  return oss.str();
}

}  // namespace bitty::AST
