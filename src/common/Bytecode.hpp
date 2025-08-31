#pragma once
#include <cstdint>
#include <string>
#include <vector>

#include "common/LiteralValue.hpp"

namespace bitty::BC {

enum class Op : uint8_t {
  NOP = 0,

  // data movement / const
  LOADK,  // A <- consts[B]
  MOVE,   // A <- R[B]

  // unary
  UNM,   // A <- -R[B]
  POS,   // A <- +R[B]
  LNOT,  // A <- !R[B]

  // binary  (A <- R[B] op R[C])
  ADD,
  SUB,
  MUL,
  DIV,
  MOD,
  EQ,
  NE,
  LT,
  LE,
  GT,
  GE,

  // control / misc
  PRINT,  // print R[A]
  JUMP,   // pc += A
  JZ,     // if !truthy(R[A]) pc += B
  JNZ,    // if truthy(R[A])  pc += B
  CALL,   // A <- call funcIndex=B with params from R[C..C+arity)
  RET,    // return R[A]
  HALT    // stop module (for $main only)
};

struct Instr {
  Op op{};
  int a{0}, b{0}, c{0};
};

struct Function {
  std::string name;
  int numRegs = 0;
  int numParams = 0;
  std::vector<LiteralValue> consts;
  std::vector<Instr> code;
};

struct Module {
  std::vector<Function> functions;
  int main = -1;  // index of $main
};

std::string disassemble(const Module& m);

}  // namespace bitty::BC
