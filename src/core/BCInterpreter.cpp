#include "core/BCInterpreter.hpp"

#include <sstream>
#include <stdexcept>
#include <vector>

#include "core/Bytecode.hpp"
#include "core/Interpreter.hpp"
#include "core/LiteralValue.hpp"

namespace bitty::BC {
using Value = LiteralValue;

namespace {
BinaryOp sym(Op op) {
  switch (op) {
    case Op::ADD:
      return BinaryOp::ADD;
    case Op::SUB:
      return BinaryOp::SUB;
    case Op::MUL:
      return BinaryOp::MUL;
    case Op::DIV:
      return BinaryOp::DIV;
    case Op::MOD:
      return BinaryOp::MOD;
    case Op::EQ:
      return BinaryOp::EQ;
    case Op::NE:
      return BinaryOp::NEQ;
    case Op::LT:
      return BinaryOp::LT;
    case Op::LE:
      return BinaryOp::LTE;
    case Op::GT:
      return BinaryOp::GT;
    case Op::GE:
      return BinaryOp::GTE;
    default:
      throw std::runtime_error("unknown Op alternative");
  }
}
static UnaryOp usym(Op op) {
  switch (op) {
    case Op::UNM:
      return UnaryOp::UNM;
    case Op::POS:
      return UnaryOp::POS;
    case Op::LNOT:
      return UnaryOp::LNOT;
    default:
      throw std::runtime_error("unknown Op alternative");
  }
}

struct Frame {
  const Function* f{};
  int ip{0};
  int retDst{-1};
  std::vector<Value> regs;
};

}  // namespace

ExecResult runBytecode(const BC::Module& m) {
  ExecResult R;
  if (m.main < 0 || m.main >= (int)m.functions.size()) {
    R.ok = false;
    R.error = "no $main";
    return R;
  }

  std::vector<Frame> stack;
  stack.reserve(16);
  auto pushFrame = [&](int fidx, int retDst, int argBase, int argCount) {
    Frame fr;
    fr.f = &m.functions[fidx];
    fr.retDst = retDst;
    fr.regs.resize(fr.f->numRegs);

    for (int i = 0; i < fr.f->numParams; ++i) {
      Value v{};
      if (i < argCount) {
        v = std::move(stack.back().regs[argBase + i]);
      }
      fr.regs[i] = std::move(v);
    }
    stack.push_back(std::move(fr));
  };

  // $main
  stack.push_back(Frame{});
  stack.back().f = &m.functions[m.main];
  stack.back().regs.resize(stack.back().f->numRegs);

  // ---------- dispatch ----------
#if defined(__clang__) || defined(__GNUC__)
#define THREADING 1
#else
#define THREADING 0
#endif

#if THREADING
#define DISPATCH()                                  \
  do {                                              \
    ins = fr->f->code[fr->ip++];                    \
    goto* labelTable[static_cast<uint8_t>(ins.op)]; \
  } while (0)
#define NEXT() DISPATCH()
#else
#define DISPATCH()              \
  do {                          \
    ins = fr->f->code[fr.ip++]; \
  } while (0)
#define NEXT() goto decode
#endif

  Value scratchV0;
  int scratchI0;
  Frame* fr = nullptr;
  BC::Instr ins;
  while (!stack.empty()) {
    fr = &stack.back();

#if THREADING
    static void* labelTable[] = {
        &&L_NOP,  &&L_LOADK, &&L_MOVE, &&L_UNM,  &&L_POS, &&L_LNOT,
        &&L_ADD,  &&L_SUB,   &&L_MUL,  &&L_DIV,  &&L_MOD, &&L_EQ,
        &&L_NE,   &&L_LT,    &&L_LE,   &&L_GT,   &&L_GE,  &&L_PRINT,
        &&L_JUMP, &&L_JZ,    &&L_JNZ,  &&L_CALL, &&L_RET, &&L_HALT};

    DISPATCH();

  L_NOP: { NEXT(); }
  L_LOADK: {
    fr->regs[ins.a] = fr->f->consts[ins.b];
    NEXT();
  }
  L_MOVE: {
    fr->regs[ins.a] = fr->regs[ins.b];
    NEXT();
  }
  L_UNM: {
    fr->regs[ins.a] = op_unary(usym(Op::UNM), fr->regs[ins.b]);
    NEXT();
  }
  L_POS: {
    fr->regs[ins.a] = op_unary(usym(Op::POS), fr->regs[ins.b]);
    NEXT();
  }
  L_LNOT: {
    fr->regs[ins.a] = op_unary(usym(Op::LNOT), fr->regs[ins.b]);
    NEXT();
  }
  L_ADD: {
    fr->regs[ins.a] = op_binary(fr->regs[ins.b], sym(Op::ADD), fr->regs[ins.c]);
    NEXT();
  }
  L_SUB: {
    fr->regs[ins.a] = op_binary(fr->regs[ins.b], sym(Op::SUB), fr->regs[ins.c]);
    NEXT();
  }
  L_MUL: {
    fr->regs[ins.a] = op_binary(fr->regs[ins.b], sym(Op::MUL), fr->regs[ins.c]);
    NEXT();
  }
  L_DIV: {
    fr->regs[ins.a] = op_binary(fr->regs[ins.b], sym(Op::DIV), fr->regs[ins.c]);
    NEXT();
  }
  L_MOD: {
    fr->regs[ins.a] = op_binary(fr->regs[ins.b], sym(Op::MOD), fr->regs[ins.c]);
    NEXT();
  }
  L_EQ: {
    fr->regs[ins.a] = op_binary(fr->regs[ins.b], sym(Op::EQ), fr->regs[ins.c]);
    NEXT();
  }
  L_NE: {
    fr->regs[ins.a] = op_binary(fr->regs[ins.b], sym(Op::NE), fr->regs[ins.c]);
    NEXT();
  }
  L_LT: {
    fr->regs[ins.a] = op_binary(fr->regs[ins.b], sym(Op::LT), fr->regs[ins.c]);
    NEXT();
  }
  L_LE: {
    fr->regs[ins.a] = op_binary(fr->regs[ins.b], sym(Op::LE), fr->regs[ins.c]);
    NEXT();
  }
  L_GT: {
    fr->regs[ins.a] = op_binary(fr->regs[ins.b], sym(Op::GT), fr->regs[ins.c]);
    NEXT();
  }
  L_GE: {
    fr->regs[ins.a] = op_binary(fr->regs[ins.b], sym(Op::GE), fr->regs[ins.c]);
    NEXT();
  }
  L_PRINT: {
    R.stdout_text += toStringRuntime(fr->regs[ins.a]);
    R.stdout_text.push_back('\n');
    NEXT();
  }
  L_JUMP: {
    fr->ip += ins.a;
    NEXT();
  }
  L_JZ: {
    if (!isTruthy(fr->regs[ins.a])) {
      fr->ip += ins.b;
    }
    NEXT();
  }
  L_JNZ: {
    if (isTruthy(fr->regs[ins.a])) {
      fr->ip += ins.b;
    }
    NEXT();
  }
  L_CALL: {
    int dst = ins.a, funcIdx = ins.b, argBase = ins.c;
    pushFrame(funcIdx, dst, argBase, m.functions[funcIdx].numParams);
    fr = &stack.back();

    NEXT();
  }
  L_RET: {
    scratchI0 = fr->retDst;
    scratchV0 = std::move(fr->regs[ins.a]);

    stack.pop_back();
    if (stack.empty()) {
      R.ok = true;
      return R;
    }

    fr = &stack.back();
    if (scratchI0 >= 0) {
      fr->regs[scratchI0] = std::move(scratchV0);
    }
    NEXT();
  }
  L_HALT: {
    R.ok = true;
    return R;
  }

#else  // SWITCH fallback
  decode:
    if (fr->ip < 0 || fr->ip >= (int)fr->f->code.size()) {
      R.ok = false;
      R.error = "pc out of range";
      return R;
    }
    DISPATCH();
    switch (ins.op) {
      case Op::NOP:
        break;
      case Op::LOADK:
        fr->regs[ins.a] = fr->f->consts[ins.b];
        break;
      case Op::MOVE:
        fr->regs[ins.a] = fr->regs[ins.b];
        break;
      case Op::UNM:
        fr->regs[ins.a] = op_unary(usym(Op::UNM), fr->regs[ins.b]);
        break;
      case Op::POS:
        fr->regs[ins.a] = op_unary(usym(Op::POS), fr->regs[ins.b]);
        break;
      case Op::LNOT:
        fr->regs[ins.a] = op_unary(usym(Op::LNOT), fr->regs[ins.b]);
        break;

      case Op::ADD:
      case Op::SUB:
      case Op::MUL:
      case Op::DIV:
      case Op::MOD:
      case Op::EQ:
      case Op::NE:
      case Op::LT:
      case Op::LE:
      case Op::GT:
      case Op::GE:
        fr->regs[ins.a] =
            op_binary(fr->regs[ins.b], sym(ins.op), fr->regs[ins.c]);
        break;

      case Op::PRINT:
        R.stdout_text += toStringRuntime(fr->regs[ins.a]);
        R.stdout_text.push_back('\n');
        break;

      case Op::JUMP:
        fr->ip += ins.a;
        goto decode;
      case Op::JZ:
        if (!isTruthy(fr->regs[ins.a])) {
          fr->ip += ins.b;
        }
        goto decode;
      case Op::JNZ:
        if (isTruthy(fr->regs[ins.a])) {
          fr->ip += ins.b;
        }
        goto decode;

      case Op::CALL: {
        int dst = ins.a, funcIdx = ins.b, argBase = ins.c;
        pushFrame(funcIdx, dst, argBase, m.functions[funcIdx].numParams);
      } break;

      case Op::RET: {
        Value ret = fr->regs[ins.a];
        stack.pop_back();
        if (stack.empty()) {
          R.ok = true;
          return R;
        }
        Frame& caller = stack.back();
        if (fr->retDst >= 0) {
          caller.regs[fr->retDst] = std::move(ret);
        }
      } break;

      case Op::HALT:
        R.ok = true;
        return R;
    }
    goto decode;
#endif
  }

  R.ok = true;
  return R;
}
}  // namespace bitty::BC
