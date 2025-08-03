#pragma once
#include <string>

#include "core/Bytecode.hpp"
#include "core/Interpreter.hpp"

namespace bitty::BC {

ExecResult runBytecode(const Module& m);

}  // namespace bitty::BC
