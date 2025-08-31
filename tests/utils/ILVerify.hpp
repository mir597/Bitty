#pragma once

#include <string>
#include <vector>

#include "common/IL.hpp"

namespace bitty::IL {
struct VerifyResult {
  bool ok = true;
  std::vector<std::string> errors;
};

VerifyResult verify(const Program& p, bool strictSymbols = false);
}  // namespace bitty::IL
