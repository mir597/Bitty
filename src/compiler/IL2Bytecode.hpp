#pragma once
#include "common/Bytecode.hpp"
#include "common/IL.hpp"

namespace bitty::BC {
// IL -> Bytecode(Module)
Module compileFromIL(const IL::Program& p);
}  // namespace bitty::BC
