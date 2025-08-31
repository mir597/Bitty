#pragma once
#include <optional>
#include <string>
#include <vector>

#include "common/AST.hpp"

struct ExecResult {
  bool ok = true;
  std::string stdout_text;
  std::string error;
};

ExecResult interpret(const bitty::AST::Program& program);

std::string toStringRuntime(const bitty::LiteralValue& v);
