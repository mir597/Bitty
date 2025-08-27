#pragma once

#include <cstdint>
#include <memory>
#include <optional>
#include <ostream>
#include <string>
#include <variant>
#include <vector>

#include "core/LiteralValue.hpp"
#include "core/Type.hpp"

namespace bitty::IL {

struct Expression;
struct Statement;
struct BlockStmt;

// e := v | x | ⊖ e | e1 ⊕ e2
struct LiteralExpr {
  LiteralValue value;
};
struct VariableExpr {
  std::string name;
};
struct UnaryExpr {
  UnaryOp op;
  std::unique_ptr<Expression> right;  // Literal or Variable
};
struct BinaryExpr {
  std::unique_ptr<Expression> left;  // Literal or Variable
  BinaryOp op;
  std::unique_ptr<Expression> right;  // Literal or Variable
};

struct PrintStmt {
  std::unique_ptr<Expression> expression;
};
struct BlockStmt {
  std::vector<std::unique_ptr<Statement>> statements;
};
struct VarDeclStmt {
  std::string name;
};
struct AssignmentStmt {
  std::string targetName;
  std::unique_ptr<Expression> value;
};
struct IfStmt {
  std::unique_ptr<Expression> condition;
  std::unique_ptr<Statement> thenBranch;
  std::unique_ptr<Statement> elseBranch;
};
struct WhileStmt {
  std::unique_ptr<Expression> condition;
  std::unique_ptr<Statement> body;
};
struct ReturnStmt {
  std::unique_ptr<Expression> value;  // optional
};
struct CallStmt {
  std::optional<std::string> targetName;
  std::string calleeName;
  std::vector<std::unique_ptr<Expression>> arguments;
};

struct FuncDeclStmt {
  std::string name;
  std::vector<std::string> params;
  std::unique_ptr<BlockStmt> body;

  std::vector<Ty::Tag> paramTypes{};
  Ty::Tag returnType = Ty::Tag::Unknown;
};

using ExpressionNode =
    std::variant<LiteralExpr, VariableExpr, UnaryExpr, BinaryExpr>;

using StatementNode =
    std::variant<PrintStmt, BlockStmt, VarDeclStmt, AssignmentStmt, IfStmt,
                 WhileStmt, ReturnStmt, CallStmt, FuncDeclStmt>;

struct Expression {
  ExpressionNode node;
  Ty::Tag type = Ty::Tag::Unknown;
};
struct Statement {
  StatementNode node;
  Ty::Tag type = Ty::Tag::Unknown;
};

struct Program {
  std::vector<std::unique_ptr<Statement>> statements;
};

std::string toSource(const Program& program);
std::string toString(const Expression& e);

std::unique_ptr<Expression> cloneExpr(const Expression& e);
std::unique_ptr<Statement> cloneStmt(const Statement& s);
std::unique_ptr<BlockStmt> cloneBlock(const BlockStmt& b);
Program cloneProgram(const Program& p);

}  // namespace bitty::IL
