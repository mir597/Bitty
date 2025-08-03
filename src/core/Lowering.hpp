#pragma once
#include "core/AST.hpp"
#include "core/IL.hpp"

namespace bitty::IL {
Program lowerFromAST(const AST::Program& ast);
}  // namespace bitty::IL
