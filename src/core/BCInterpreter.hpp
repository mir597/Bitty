#pragma once
#include <string>

#include "common/Bytecode.hpp"
#include "interpreter/Interpreter.hpp"

namespace bitty::BC {

ExecResult runBytecode(const Module& m);

}  // namespace bitty::BC
