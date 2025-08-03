#include "core/Bytecode.hpp"

#include <iomanip>
#include <sstream>

#include "core/Interpreter.hpp"  // toStringRuntime()

namespace bitty::BC {

static const char* opName(Op op) {
  switch (op) {
    case Op::NOP:
      return "NOP";
    case Op::LOADK:
      return "LOADK";
    case Op::MOVE:
      return "MOVE";
    case Op::UNM:
      return "UNM";
    case Op::POS:
      return "POS";
    case Op::LNOT:
      return "LNOT";
    case Op::ADD:
      return "ADD";
    case Op::SUB:
      return "SUB";
    case Op::MUL:
      return "MUL";
    case Op::DIV:
      return "DIV";
    case Op::MOD:
      return "MOD";
    case Op::EQ:
      return "EQ";
    case Op::NE:
      return "NE";
    case Op::LT:
      return "LT";
    case Op::LE:
      return "LE";
    case Op::GT:
      return "GT";
    case Op::GE:
      return "GE";
    case Op::PRINT:
      return "PRINT";
    case Op::JUMP:
      return "JUMP";
    case Op::JZ:
      return "JZ";
    case Op::JNZ:
      return "JNZ";
    case Op::CALL:
      return "CALL";
    case Op::RET:
      return "RET";
    case Op::HALT:
      return "HALT";
  }
  return "?";
}

std::string disassemble(const Module& m) {
  std::ostringstream out;

  for (size_t fi = 0; fi < m.functions.size(); ++fi) {
    auto const& f = m.functions[fi];
    out << "fn " << f.name << "(params=" << f.numParams
        << ", regs=" << f.numRegs << ")\n";

    if (!f.consts.empty()) {
      out << "  .consts:\n";
      for (size_t i = 0; i < f.consts.size(); ++i) {
        out << "    [" << i << "] = " << toStringRuntime(f.consts[i]) << "\n";
      }
    }

    out << "  .code:\n";
    for (size_t ip = 0; ip < f.code.size(); ++ip) {
      auto const& ins = f.code[ip];
      out << "    " << std::setw(4) << ip << ": " << opName(ins.op);
      switch (ins.op) {
        case Op::LOADK:
          out << "  R" << ins.a << " <- K[" << ins.b << "]";
          break;
        case Op::MOVE:
          out << "  R" << ins.a << " <- R" << ins.b;
          break;

        case Op::UNM:
        case Op::POS:
        case Op::LNOT:
          out << "  R" << ins.a << " <- op R" << ins.b;
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
          out << "  R" << ins.a << " <- R" << ins.b << " ? R" << ins.c;
          break;

        case Op::PRINT:
          out << "  R" << ins.a;
          break;

        case Op::JUMP:
          out << "  +" << ins.a;
          break;
        case Op::JZ:
        case Op::JNZ:
          out << "  R" << ins.a << " +" << ins.b;
          break;

        case Op::CALL:
          out << "  R" << ins.a << " <- call f#" << ins.b << " args@R" << ins.c;
          break;

        case Op::RET:
          out << "  R" << ins.a;
          break;
        case Op::HALT:
          break;
        case Op::NOP:
          break;
      }
      out << "\n";
    }
    if ((int)fi == m.main) {
      out << "  ; ^-- $main\n";
    }
    out << "\n";
  }

  return out.str();
}

}  // namespace bitty::BC
