#pragma once

#include <memory>
#include <sstream>
#include <string>
#include <vector>

#include "BittyLexer.h"
#include "BittyParser.h"
#include "core/AST.hpp"
#include "core/ASTBuilder.hpp"

using namespace bitty;

static bool eqExpr(const AST::Expression& a, const AST::Expression& b);
static bool eqStmt(const AST::Statement& a, const AST::Statement& b);
static bool eqBlock(const AST::BlockStmt& a, const AST::BlockStmt& b);

static bool eqLiteral(const LiteralValue& a, const LiteralValue& b) {
  if (a.index() != b.index()) {
    return false;
  }
  return std::visit(
      [&](auto const& x) -> bool {
        using T = std::decay_t<decltype(x)>;
        const T* py = std::get_if<T>(&b);
        if (!py) {
          return false;
        }

        if constexpr (std::is_same_v<T, std::monostate>) {
          return true;  // nil
        } else if constexpr (std::is_same_v<T, bool>) {
          return x == *py;
        } else if constexpr (std::is_same_v<T, int64_t>) {
          return x == *py;
        } else if constexpr (std::is_same_v<T, double>) {
          double y = *py;
          double diff = std::abs(x - y);
          double scale =
              std::max<double>(1.0, std::max(std::abs(x), std::abs(y)));
          return diff <= 1e-12 * scale;
        } else if constexpr (std::is_same_v<T, std::string>) {
          return x == *py;
        } else {
          static_assert(!sizeof(T*), "Unhandled LiteralValue alternative");
          return false;
        }
      },
      a);
}

static bool eqExprNode(const AST::ExpressionNode& A,
                       const AST::ExpressionNode& B) {
  if (A.index() != B.index()) {
    return false;
  }
  return std::visit(
      [&](auto const& nodeA) -> bool {
        using T = std::decay_t<decltype(nodeA)>;
        const T* pB = std::get_if<T>(&B);
        if (!pB) {
          return false;
        }
        const T& nodeB = *pB;

        if constexpr (std::is_same_v<T, AST::LiteralExpr>) {
          return eqLiteral(nodeA.value, nodeB.value);
        } else if constexpr (std::is_same_v<T, AST::VariableExpr>) {
          return nodeA.name == nodeB.name;
        } else if constexpr (std::is_same_v<T, AST::UnaryExpr>) {
          return nodeA.op == nodeB.op && eqExpr(*nodeA.right, *nodeB.right);
        } else if constexpr (std::is_same_v<T, AST::BinaryExpr>) {
          return nodeA.op == nodeB.op && eqExpr(*nodeA.left, *nodeB.left) &&
                 eqExpr(*nodeA.right, *nodeB.right);
        } else if constexpr (std::is_same_v<T, AST::AssignmentExpr>) {
          return eqExpr(*nodeA.target, *nodeB.target) &&
                 eqExpr(*nodeA.value, *nodeB.value);
        } else if constexpr (std::is_same_v<T, AST::CallExpr>) {
          if (!eqExpr(*nodeA.callee, *nodeB.callee)) {
            return false;
          }
          if (nodeA.arguments.size() != nodeB.arguments.size()) {
            return false;
          }
          for (size_t i = 0; i < nodeA.arguments.size(); ++i) {
            if (!eqExpr(*nodeA.arguments[i], *nodeB.arguments[i])) {
              return false;
            }
          }
          return true;
        } else {
          static_assert(!sizeof(T*), "Unhandled Expression node");
          return false;
        }
      },
      A);
}

static bool eqExpr(const AST::Expression& a, const AST::Expression& b) {
  return eqExprNode(a.node, b.node);
}

static bool eqStmtNode(const AST::StatementNode& A,
                       const AST::StatementNode& B) {
  if (A.index() != B.index()) {
    return false;
  }
  return std::visit(
      [&](auto const& a) -> bool {
        using T = std::decay_t<decltype(a)>;
        const T* pB = std::get_if<T>(&B);
        if (!pB) {
          return false;
        }
        const T& b = *pB;

        if constexpr (std::is_same_v<T, AST::ExpressionStmt>) {
          return eqExpr(*a.expression, *b.expression);
        } else if constexpr (std::is_same_v<T, AST::PrintStmt>) {
          return eqExpr(*a.expression, *b.expression);
        } else if constexpr (std::is_same_v<T, AST::BlockStmt>) {
          return eqBlock(a, b);
        } else if constexpr (std::is_same_v<T, AST::VarDeclStmt>) {
          if (a.name != b.name) {
            return false;
          }
          bool ai = (bool)a.initializer, bi = (bool)b.initializer;
          if (ai != bi) {
            return false;
          }
          return ai ? eqExpr(*a.initializer, *b.initializer) : true;
        } else if constexpr (std::is_same_v<T, AST::IfStmt>) {
          if (!eqExpr(*a.condition, *b.condition)) {
            return false;
          }
          if (!eqStmt(*a.thenBranch, *b.thenBranch)) {
            return false;
          }
          bool ae = (bool)a.elseBranch, be = (bool)b.elseBranch;
          if (ae != be) {
            return false;
          }
          return ae ? eqStmt(*a.elseBranch, *b.elseBranch) : true;
        } else if constexpr (std::is_same_v<T, AST::WhileStmt>) {
          return eqExpr(*a.condition, *b.condition) && eqStmt(*a.body, *b.body);
        } else if constexpr (std::is_same_v<T, AST::ReturnStmt>) {
          bool av = (bool)a.value, bv = (bool)b.value;
          if (av != bv) {
            return false;
          }
          return av ? eqExpr(*a.value, *b.value) : true;
        } else if constexpr (std::is_same_v<T, AST::FuncDeclStmt>) {
          if (a.name != b.name) {
            return false;
          }
          if (a.params != b.params) {
            return false;
          }
          return eqBlock(*a.body, *b.body);
        } else {
          static_assert(!sizeof(T*), "Unhandled Statement node");
          return false;
        }
      },
      A);
}

static bool eqStmt(const AST::Statement& a, const AST::Statement& b) {
  return eqStmtNode(a.node, b.node);
}

static bool eqBlock(const AST::BlockStmt& a, const AST::BlockStmt& b) {
  if (a.statements.size() != b.statements.size()) {
    return false;
  }
  for (size_t i = 0; i < a.statements.size(); ++i) {
    if (!eqStmt(*a.statements[i], *b.statements[i])) {
      return false;
    }
  }
  return true;
}

static bool eqProgram(const AST::Program& a, const AST::Program& b) {
  if (a.statements.size() != b.statements.size()) {
    return false;
  }
  for (size_t i = 0; i < a.statements.size(); ++i) {
    if (!eqStmt(*a.statements[i], *b.statements[i])) {
      return false;
    }
  }
  return true;
}
