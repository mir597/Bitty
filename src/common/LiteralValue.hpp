#pragma once

#include <variant>

namespace bitty {
using LiteralValue =
    std::variant<std::monostate, bool, int64_t, double, std::string>;

bool parseNumber(const std::string& s, double& out);
double toNumber(const LiteralValue& v);
std::string valueToString(const LiteralValue& v);
int64_t toInt(const LiteralValue& v);
bool isTruthy(const LiteralValue& v);
LiteralValue repeatString(const std::string& s, int64_t n);
bool bothString(const LiteralValue& a, const LiteralValue& b);
bool bothInt(const LiteralValue& a, const LiteralValue& b);

enum class UnaryOp {
  UNM,   // -e
  LNOT,  // !e
  POS,   // +e
};

enum class BinaryOp {
  ADD,
  SUB,
  MUL,
  DIV,
  MOD,
  EQ,
  NEQ,
  LT,
  LTE,
  GT,
  GTE,
  AND,
  OR
};

UnaryOp toUnaryOp(const std::string& op);
BinaryOp toBinaryOp(const std::string& op);
std::string toString(const UnaryOp& op);
std::string toString(const BinaryOp& op);

LiteralValue op_unary(const UnaryOp& op, const LiteralValue& rhs);
LiteralValue op_binary(const LiteralValue& a, const BinaryOp& op,
                       const LiteralValue& b);

}  // namespace bitty
