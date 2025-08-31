#include "common/IL.hpp"

#include <iomanip>
#include <limits>
#include <sstream>

namespace bitty::IL {
namespace {
std::string typeSuffix(Ty::Tag t) {
  if (t == Ty::Tag::Unknown) {
    return "";
  }
  std::ostringstream oss;
  oss << ": " << Ty::toString(t);
  return oss.str();
}

void indent(std::ostream& os, int n) {
  while (n--) {
    os << "  ";
  }
}

std::string formatDouble(double x) {
  std::ostringstream oss;
  oss << std::setprecision(std::numeric_limits<double>::max_digits10) << x;
  std::string s = oss.str();
  if (s.find_first_of("eE") != std::string::npos) {
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
      [&](auto const& x) {
        using T = std::decay_t<decltype(x)>;
        if constexpr (std::is_same_v<T, std::monostate>) {
          os << "nil";
        } else if constexpr (std::is_same_v<T, bool>) {
          os << (x ? "true" : "false");
        } else if constexpr (std::is_same_v<T, int64_t>) {
          os << x;
        } else if constexpr (std::is_same_v<T, double>) {
          os << formatDouble(x);
        } else if constexpr (std::is_same_v<T, std::string>) {
          os << "\"";
          for (char c : x) {
            switch (c) {
              case '\\':
                os << "\\\\";
                break;
              case '"':
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
          static_assert(!sizeof(T*), "bad literal");
        }
      },
      v);
}

enum Prec {
  P_LOW = 0,
  P_ASSIGN = 1,
  P_OR = 2,
  P_AND = 3,
  P_EQ = 4,
  P_CMP = 5,
  P_ADD = 6,
  P_MUL = 7,
  P_UNARY = 8,
  P_PRIMARY = 9
};

int precOf(const BinaryOp& op) {
  switch (op) {
    case BinaryOp::OR:
      return P_OR;
    case BinaryOp::AND:
      return P_AND;
    case BinaryOp::EQ:
    case BinaryOp::NEQ:
      return P_EQ;
    case BinaryOp::LT:
    case BinaryOp::LTE:
    case BinaryOp::GT:
    case BinaryOp::GTE:
      return P_CMP;
    case BinaryOp::ADD:
    case BinaryOp::SUB:
      return P_ADD;
    case BinaryOp::MUL:
    case BinaryOp::DIV:
      return P_MUL;
    default:
      return P_PRIMARY;
  }
}

void emitExpr(const IL::Expression& e, std::ostream& os,
              int parentPrec = P_LOW);

void emitExprNode(const IL::ExpressionNode& n, std::ostream& os,
                  int parentPrec) {
  std::visit(
      [&](auto const& node) {
        using T = std::decay_t<decltype(node)>;
        if constexpr (std::is_same_v<T, IL::LiteralExpr>) {
          emitLiteral(node.value, os);
        } else if constexpr (std::is_same_v<T, IL::VariableExpr>) {
          os << node.name;
        } else if constexpr (std::is_same_v<T, IL::UnaryExpr>) {
          int my = P_UNARY;
          bool paren = my < parentPrec;
          if (paren) {
            os << "(";
          }
          os << toString(node.op);
          emitExpr(*node.right, os, my);
          if (paren) {
            os << ")";
          }
        } else if constexpr (std::is_same_v<T, IL::BinaryExpr>) {
          int my = precOf(node.op);
          bool paren = my < parentPrec;
          if (paren) {
            os << "(";
          }
          emitExpr(*node.left, os, my);
          os << " " << toString(node.op) << " ";
          emitExpr(*node.right, os, my);
          if (paren) {
            os << ")";
          }
        } else {
          static_assert(!sizeof(T*), "bad expr");
        }
      },
      n);
}

void emitExpr(const IL::Expression& e, std::ostream& os, int parentPrec) {
  emitExprNode(e.node, os, parentPrec);
}

void emitBlock(const IL::BlockStmt& b, std::ostream& os, int lvl);

void emitStmt(const IL::Statement& s, std::ostream& os, int lvl) {
  std::visit(
      [&](auto const& st) {
        using T = std::decay_t<decltype(st)>;
        if constexpr (std::is_same_v<T, IL::PrintStmt>) {
          indent(os, lvl);
          os << "print ";
          emitExpr(*st.expression, os, P_ASSIGN);
          os << typeSuffix(st.expression->type);
          os << ";\n";
        } else if constexpr (std::is_same_v<T, IL::BlockStmt>) {
          emitBlock(st, os, lvl);
          os << "\n";
        } else if constexpr (std::is_same_v<T, IL::VarDeclStmt>) {
          indent(os, lvl);
          os << "let " << st.name << typeSuffix(s.type) << ";\n";
        } else if constexpr (std::is_same_v<T, IL::AssignmentStmt>) {
          indent(os, lvl);
          os << st.targetName << " = ";
          emitExpr(*st.value, os, P_ASSIGN);
          os << typeSuffix(st.value->type);
          os << ";\n";
        } else if constexpr (std::is_same_v<T, IL::IfStmt>) {
          indent(os, lvl);
          os << "if (";
          emitExpr(*st.condition, os, P_ASSIGN);
          os << ") ";
          emitBlock(std::get<IL::BlockStmt>(st.thenBranch->node), os, lvl);
          if (st.elseBranch) {
            os << " else ";
            emitBlock(std::get<IL::BlockStmt>(st.elseBranch->node), os, lvl);
          }
          os << "\n";
        } else if constexpr (std::is_same_v<T, IL::WhileStmt>) {
          indent(os, lvl);
          os << "while (";
          emitExpr(*st.condition, os, P_ASSIGN);
          os << typeSuffix(st.condition->type);
          os << ") ";
          emitBlock(std::get<IL::BlockStmt>(st.body->node), os, lvl);
          os << "\n";
        } else if constexpr (std::is_same_v<T, IL::ReturnStmt>) {
          indent(os, lvl);
          os << "return";
          if (st.value) {
            os << " ";
            emitExpr(*st.value, os, P_ASSIGN);
            os << typeSuffix(st.value->type);
          }
          os << ";\n";
        } else if constexpr (std::is_same_v<T, IL::CallStmt>) {
          indent(os, lvl);
          if (st.targetName) {
            os << *st.targetName << " = ";
          }
          os << st.calleeName << "(";
          for (size_t i = 0; i < st.arguments.size(); ++i) {
            if (i) {
              os << ", ";
            }
            emitExpr(*st.arguments[i], os, P_ASSIGN);
            os << typeSuffix(st.arguments[i]->type);
          }
          os << ");\n";
        } else if constexpr (std::is_same_v<T, IL::FuncDeclStmt>) {
          indent(os, lvl);
          os << "fn " << st.name << "(";
          for (size_t i = 0; i < st.params.size(); ++i) {
            if (i) {
              os << ", ";
            }
            os << st.params[i];
            if (i < st.paramTypes.size()) {
              os << typeSuffix(st.paramTypes[i]);
            }
          }
          os << ")";
          os << typeSuffix(st.returnType);
          os << " ";
          emitBlock(*st.body, os, lvl);
          os << "\n";
        } else {
          static_assert(!sizeof(T*), "bad stmt");
        }
      },
      s.node);
}

void emitBlock(const IL::BlockStmt& b, std::ostream& os, int lvl) {
  os << "{\n";
  for (auto const& sp : b.statements) {
    emitStmt(*sp, os, lvl + 1);
  }
  indent(os, lvl);
  os << "}";
}

}  // namespace

std::string toString(const Expression& e) {
  std::ostringstream oss;
  emitExpr(e, oss, P_LOW);
  return oss.str();
}

std::string toSource(const Program& p) {
  std::ostringstream oss;
  for (auto const& st : p.statements) {
    emitStmt(*st, oss, 0);
  }
  return oss.str();
}

std::unique_ptr<Expression> cloneExpr(const Expression& e) {
  auto out = std::make_unique<Expression>();
  out->type = e.type;
  out->node = std::visit(
      [&](auto const& node) -> ExpressionNode {
        using T = std::decay_t<decltype(node)>;
        if constexpr (std::is_same_v<T, LiteralExpr>) {
          return node;
        } else if constexpr (std::is_same_v<T, VariableExpr>) {
          return node;
        } else if constexpr (std::is_same_v<T, UnaryExpr>) {
          return UnaryExpr{.op = node.op, .right = cloneExpr(*node.right)};
        } else if constexpr (std::is_same_v<T, BinaryExpr>) {
          return BinaryExpr{.op = node.op,
                            .left = cloneExpr(*node.left),
                            .right = cloneExpr(*node.right)};
        } else {
          static_assert(!sizeof(T*),
                        "Unhandled IL::Expression alternative in cloneExpr()");
        }
      },
      e.node);
  return out;
}

std::unique_ptr<Statement> cloneStmt(const Statement& s) {
  auto out = std::make_unique<Statement>();
  out->type = s.type;
  out->node = std::visit(
      [&](auto const& st) -> StatementNode {
        using T = std::decay_t<decltype(st)>;
        if constexpr (std::is_same_v<T, PrintStmt>) {
          return PrintStmt{cloneExpr(*st.expression)};
        } else if constexpr (std::is_same_v<T, BlockStmt>) {
          std::vector<std::unique_ptr<Statement>> stmts;
          stmts.reserve(st.statements.size());
          for (auto const& sp : st.statements) {
            stmts.push_back(cloneStmt(*sp));
          }
          return BlockStmt{std::move(stmts)};
        } else if constexpr (std::is_same_v<T, VarDeclStmt>) {
          return st;
        } else if constexpr (std::is_same_v<T, AssignmentStmt>) {
          return AssignmentStmt{.targetName = st.targetName,
                                .value = cloneExpr(*st.value)};
        } else if constexpr (std::is_same_v<T, IfStmt>) {
          return IfStmt{.condition = cloneExpr(*st.condition),
                        .thenBranch = cloneStmt(*st.thenBranch),
                        .elseBranch = st.elseBranch ? cloneStmt(*st.elseBranch)
                                                    : nullptr};
        } else if constexpr (std::is_same_v<T, WhileStmt>) {
          return WhileStmt{.condition = cloneExpr(*st.condition),
                           .body = cloneStmt(*st.body)};
        } else if constexpr (std::is_same_v<T, ReturnStmt>) {
          return ReturnStmt{.value = st.value ? cloneExpr(*st.value) : nullptr};
        } else if constexpr (std::is_same_v<T, CallStmt>) {
          std::vector<std::unique_ptr<Expression>> arguments;
          arguments.reserve(st.arguments.size());
          for (auto const& arg : st.arguments) {
            arguments.push_back(cloneExpr(*arg));
          }
          return CallStmt{.targetName = st.targetName,
                          .calleeName = st.calleeName,
                          .arguments = std::move(arguments)};
        } else if constexpr (std::is_same_v<T, FuncDeclStmt>) {
          return FuncDeclStmt{.name = st.name,
                              .params = st.params,
                              .paramTypes = st.paramTypes,
                              .returnType = st.returnType,
                              .body = cloneBlock(*st.body)};
        } else {
          static_assert(!sizeof(T*),
                        "Unhandled IL::Statement alternative in cloneStmt()");
        }
      },
      s.node);
  return out;
}

std::unique_ptr<BlockStmt> cloneBlock(const BlockStmt& b) {
  auto out = std::make_unique<BlockStmt>();
  out->statements.reserve(b.statements.size());
  for (auto const& sp : b.statements) {
    out->statements.push_back(cloneStmt(*sp));
  }
  return out;
}

Program cloneProgram(const Program& p) {
  Program q;
  q.statements.reserve(p.statements.size());
  for (auto const& sp : p.statements) {
    q.statements.push_back(cloneStmt(*sp));
  }
  return q;
}

}  // namespace bitty::IL
