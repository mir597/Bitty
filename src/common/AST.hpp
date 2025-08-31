#pragma once

#include <cstdint>
#include <memory>
#include <optional>
#include <ostream>
#include <string>
#include <variant>
#include <vector>

#include "common/LiteralValue.hpp"

namespace bitty::AST {

struct Expression;
struct Statement;
struct BlockStmt;

// Expression
// e := v | x | ⊖ e | e1 ⊕ e2 | e1 = e2 | e(e_i...)
struct LiteralExpr {
  LiteralValue value;
};
struct VariableExpr {
  std::string name;
};
struct UnaryExpr {
  UnaryOp op;
  std::unique_ptr<Expression> right;
};
struct BinaryExpr {
  std::unique_ptr<Expression> left;
  BinaryOp op;
  std::unique_ptr<Expression> right;
};
struct AssignmentExpr {
  std::unique_ptr<Expression> target;
  std::unique_ptr<Expression> value;
};
struct CallExpr {
  std::unique_ptr<Expression> callee;
  std::vector<std::unique_ptr<Expression>> arguments;
};

using ExpressionNode = std::variant<LiteralExpr, VariableExpr, UnaryExpr,
                                    BinaryExpr, AssignmentExpr, CallExpr>;

// ===== Statements =====
struct ExpressionStmt {
  std::unique_ptr<Expression> expression;
};
struct PrintStmt {
  std::unique_ptr<Expression> expression;
};
struct BlockStmt {
  std::vector<std::unique_ptr<Statement>> statements;
};
struct VarDeclStmt {
  std::string name;
  std::unique_ptr<Expression> initializer;  // optional
};
struct IfStmt {
  std::unique_ptr<Expression> condition;
  std::unique_ptr<Statement> thenBranch;
  std::unique_ptr<Statement> elseBranch;  // optional
};
struct WhileStmt {
  std::unique_ptr<Expression> condition;
  std::unique_ptr<Statement> body;
};
struct ReturnStmt {
  std::unique_ptr<Expression> value;
};  // optional

struct FuncDeclStmt {
  std::string name;
  std::vector<std::string> params;
  std::unique_ptr<BlockStmt> body;
};

using StatementNode =
    std::variant<ExpressionStmt, PrintStmt, BlockStmt, VarDeclStmt, IfStmt,
                 WhileStmt, ReturnStmt, FuncDeclStmt>;

struct Expression {
  ExpressionNode node;
};
struct Statement {
  StatementNode node;
};

struct Program {
  std::vector<std::unique_ptr<Statement>> statements;
};

void format(const Program& program, std::ostream& os, int indent = 0);
std::string toSource(const Program& program);
}  // namespace bitty::AST
