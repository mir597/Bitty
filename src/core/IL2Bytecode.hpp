#pragma once
#include "core/Bytecode.hpp"
#include "core/IL.hpp"

namespace bitty::BC {
// IL -> Bytecode(Module)
Module compileFromIL(const IL::Program& p);
}  // namespace bitty::BC
