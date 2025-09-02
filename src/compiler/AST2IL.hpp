#pragma once
#include "common/AST.hpp"
#include "common/IL.hpp"

namespace bitty::IL {
Program lowerFromAST(const AST::Program& ast);
}  // namespace bitty::IL
