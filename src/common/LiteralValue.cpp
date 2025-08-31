#include "common/LiteralValue.hpp"

#include <sstream>
#include <string>
#include <variant>

namespace bitty {
UnaryOp toUnaryOp(const std::string& op) {
  if (op == "+") {
    return UnaryOp::POS;
  } else if (op == "-") {
    return UnaryOp::UNM;
  } else if (op == "!") {
    return UnaryOp::LNOT;
  }
  throw std::invalid_argument("Invalid unary operator");
}
BinaryOp toBinaryOp(const std::string& op) {
  if (op == "+") {
    return BinaryOp::ADD;
  } else if (op == "-") {
    return BinaryOp::SUB;
  } else if (op == "*") {
    return BinaryOp::MUL;
  } else if (op == "/") {
    return BinaryOp::DIV;
  } else if (op == "%") {
    return BinaryOp::MOD;
  } else if (op == "==") {
    return BinaryOp::EQ;
  } else if (op == "!=") {
    return BinaryOp::NEQ;
  } else if (op == "<") {
    return BinaryOp::LT;
  } else if (op == "<=") {
    return BinaryOp::LTE;
  } else if (op == ">") {
    return BinaryOp::GT;
  } else if (op == ">=") {
    return BinaryOp::GTE;
  } else if (op == "&&") {
    return BinaryOp::AND;
  } else if (op == "||") {
    return BinaryOp::OR;
  }
  throw std::invalid_argument("Invalid binary operator");
}

std::string toString(const UnaryOp& op) {
  switch (op) {
    case UnaryOp::UNM:
      return "-";
    case UnaryOp::POS:
      return "+";
    case UnaryOp::LNOT:
      return "!";
  }
  throw std::invalid_argument("Invalid unary operator");
}

std::string toString(const BinaryOp& op) {
  switch (op) {
    case BinaryOp::ADD:
      return "+";
    case BinaryOp::SUB:
      return "-";
    case BinaryOp::MUL:
      return "*";
    case BinaryOp::DIV:
      return "/";
    case BinaryOp::MOD:
      return "%";
    case BinaryOp::EQ:
      return "==";
    case BinaryOp::NEQ:
      return "!=";
    case BinaryOp::LT:
      return "<";
    case BinaryOp::LTE:
      return "<=";
    case BinaryOp::GT:
      return ">";
    case BinaryOp::GTE:
      return ">=";
    case BinaryOp::AND:
      return "&&";
    case BinaryOp::OR:
      return "||";
  }
  throw std::invalid_argument("Invalid binary operator");
}

using LiteralValue =
    std::variant<std::monostate, bool, int64_t, double, std::string>;

bool parseNumber(const std::string& s, double& out) {
  try {
    size_t idx = 0;
    out = std::stod(s, &idx);
    if (idx != s.size()) {
      return false;
    }
    return true;
  } catch (...) {
    return false;
  }
}

double toNumber(const LiteralValue& v) {
  if (std::holds_alternative<std::monostate>(v)) {
    return 0.0;
  } else if (std::holds_alternative<bool>(v)) {
    return std::get<bool>(v) ? 1.0 : 0.0;
  } else if (std::holds_alternative<int64_t>(v)) {
    return static_cast<double>(std::get<int64_t>(v));
  } else if (std::holds_alternative<double>(v)) {
    return std::get<double>(v);
  } else if (std::holds_alternative<std::string>(v)) {
    double d;
    return parseNumber(std::get<std::string>(v), d) ? d : 0.0;
  }
  return 0.0;
}

std::string valueToString(const LiteralValue& v) {
  if (std::holds_alternative<std::monostate>(v)) {
    return "null";
  } else if (std::holds_alternative<bool>(v)) {
    return std::get<bool>(v) ? "true" : "false";
  } else if (std::holds_alternative<int64_t>(v)) {
    return std::to_string(std::get<int64_t>(v));
  } else if (std::holds_alternative<double>(v)) {
    std::ostringstream oss;
    oss << std::get<double>(v);
    return oss.str();
  } else if (std::holds_alternative<std::string>(v)) {
    return std::get<std::string>(v);
  }
  return "";
}

int64_t toInt(const LiteralValue& v) {
  if (std::holds_alternative<int64_t>(v)) {
    return std::get<int64_t>(v);
  }
  double d = toNumber(v);
  return static_cast<int64_t>(d);
}

bool isTruthy(const LiteralValue& v) {
  if (std::holds_alternative<std::monostate>(v)) {
    return false;
  } else if (std::holds_alternative<bool>(v)) {
    return std::get<bool>(v);
  } else if (std::holds_alternative<int64_t>(v)) {
    return std::get<int64_t>(v) != 0;
  } else if (std::holds_alternative<double>(v)) {
    return std::get<double>(v) != 0.0;
  } else if (std::holds_alternative<std::string>(v)) {
    return !std::get<std::string>(v).empty();
  }
  return false;
}

LiteralValue repeatString(const std::string& s, int64_t n) {
  if (n <= 0) {
    return std::string{};
  } else if (n == 1) {
    return s;
  }
  std::string out;
  out.reserve(s.size() * size_t(n));
  for (int64_t i = 0; i < n; ++i) {
    out += s;
  }
  return out;
}

LiteralValue op_unary(const UnaryOp& op, const LiteralValue& rhs) {
  switch (op) {
    case UnaryOp::UNM:
      if (std::holds_alternative<int64_t>(rhs)) {
        return LiteralValue{-std::get<int64_t>(rhs)};
      }
      return LiteralValue{-toNumber(rhs)};
    case UnaryOp::POS:
      if (std::holds_alternative<int64_t>(rhs)) {
        return LiteralValue{+std::get<int64_t>(rhs)};
      }
      return LiteralValue{+toNumber(rhs)};
    case UnaryOp::LNOT:
      return LiteralValue{!isTruthy(rhs)};
  }
}

bool bothString(const LiteralValue& a, const LiteralValue& b) {
  return std::holds_alternative<std::string>(a) &&
         std::holds_alternative<std::string>(b);
}
bool bothInt(const LiteralValue& a, const LiteralValue& b) {
  return std::holds_alternative<int64_t>(a) &&
         std::holds_alternative<int64_t>(b);
}

LiteralValue op_binary(const LiteralValue& a, const BinaryOp& op,
                       const LiteralValue& b) {
  auto cmp_num = [&](auto rel) -> LiteralValue {
    if (bothString(a, b)) {
      const auto& sa = std::get<std::string>(a);
      const auto& sb = std::get<std::string>(b);
      return LiteralValue{rel(sa<sb, sa <= sb, sa> sb, sa >= sb, sa == sb)};
    } else {
      double da = toNumber(a), db = toNumber(b);
      return LiteralValue{rel(da<db, da <= db, da> db, da >= db, da == db)};
    }
  };

  switch (op) {
    case BinaryOp::AND:
      return LiteralValue{isTruthy(a) && isTruthy(b)};
    case BinaryOp::OR:
      return LiteralValue{isTruthy(a) || isTruthy(b)};
    case BinaryOp::ADD:
      if (std::holds_alternative<std::string>(a) ||
          std::holds_alternative<std::string>(b)) {
        return LiteralValue{valueToString(a) + valueToString(b)};
      }
      if (bothInt(a, b)) {
        return LiteralValue{std::get<int64_t>(a) + std::get<int64_t>(b)};
      }
      return LiteralValue{toNumber(a) + toNumber(b)};
    case BinaryOp::SUB:
      if (bothInt(a, b)) {
        return LiteralValue{std::get<int64_t>(a) - std::get<int64_t>(b)};
      }
      return LiteralValue{toNumber(a) - toNumber(b)};
    case BinaryOp::MUL:
      if (std::holds_alternative<std::string>(a) &&
          (std::holds_alternative<int64_t>(b) ||
           std::holds_alternative<double>(b))) {
        return repeatString(std::get<std::string>(a), toInt(b));
      } else if (std::holds_alternative<std::string>(b) &&
                 (std::holds_alternative<int64_t>(a) ||
                  std::holds_alternative<double>(a))) {
        return repeatString(std::get<std::string>(b), toInt(a));
      } else if (bothInt(a, b)) {
        return LiteralValue{std::get<int64_t>(a) * std::get<int64_t>(b)};
      }
      return LiteralValue{toNumber(a) * toNumber(b)};
    case BinaryOp::DIV:
      return LiteralValue{toNumber(a) / toNumber(b)};
    case BinaryOp::MOD: {
      int64_t ai = toInt(a), bi = toInt(b);
      return LiteralValue{bi == 0 ? int64_t(0) : int64_t(ai % bi)};
    }
    case BinaryOp::EQ:
    case BinaryOp::NEQ: {
      bool eq = false;
      if (bothString(a, b)) {
        eq = (std::get<std::string>(a) == std::get<std::string>(b));
      } else if (bothInt(a, b)) {
        eq = (std::get<int64_t>(a) == std::get<int64_t>(b));
      } else if (std::holds_alternative<double>(a) ||
                 std::holds_alternative<double>(b) ||
                 std::holds_alternative<int64_t>(a) ||
                 std::holds_alternative<int64_t>(b) ||
                 std::holds_alternative<bool>(a) ||
                 std::holds_alternative<bool>(b) ||
                 std::holds_alternative<std::string>(a) ||
                 std::holds_alternative<std::string>(b) ||
                 std::holds_alternative<std::monostate>(a) ||
                 std::holds_alternative<std::monostate>(b)) {
        eq = (toNumber(a) == toNumber(b));
      }
      return LiteralValue{op == BinaryOp::EQ ? eq : !eq};
    }
    case BinaryOp::LT:
      return cmp_num([&](bool lt, bool, bool, bool, bool) { return lt; });
    case BinaryOp::LTE:
      return cmp_num([&](bool, bool le, bool, bool, bool) { return le; });
    case BinaryOp::GT:
      return cmp_num([&](bool, bool, bool gt, bool, bool) { return gt; });
    case BinaryOp::GTE:
      return cmp_num([&](bool, bool, bool, bool ge, bool) { return ge; });
    default:
      throw std::runtime_error("unsupported binary op: " + toString(op));
  }
}

}  // namespace bitty
