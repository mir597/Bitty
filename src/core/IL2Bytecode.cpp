#include "core/IL2Bytecode.hpp"

#include <functional>
#include <stdexcept>
#include <unordered_map>
#include <unordered_set>

#include "core/LiteralValue.hpp"

using namespace bitty;
using namespace bitty::BC;

namespace {

struct FnBuilder {
  Function f;
  std::vector<std::unordered_map<std::string, int>> scopes;
  std::unordered_map<std::string, int> constIx;

  int addConst(const LiteralValue& v) {
    std::string k;
    if (std::holds_alternative<std::monostate>(v)) {
      k = "nil";
    } else if (std::holds_alternative<bool>(v)) {
      k = std::get<bool>(v) ? "true" : "false";
    } else if (std::holds_alternative<int64_t>(v)) {
      k = "i:" + std::to_string(std::get<int64_t>(v));
    } else if (std::holds_alternative<double>(v)) {
      k = "d:" + std::to_string(std::get<double>(v));
    } else {
      k = "s:" + std::get<std::string>(v);
    }

    auto it = constIx.find(k);
    if (it != constIx.end()) {
      return it->second;
    }
    int id = (int)f.consts.size();
    f.consts.push_back(v);
    constIx.emplace(k, id);
    return id;
  }

  void push() {
    scopes.emplace_back();
  }
  void pop() {
    scopes.pop_back();
  }

  int declare(const std::string& n) {
    if (scopes.empty()) {
      push();
    }
    auto& cur = scopes.back();
    auto it = cur.find(n);
    if (it != cur.end()) {
      return it->second;
    }
    int r = f.numRegs++;
    cur[n] = r;
    return r;
  }

  int lookup(const std::string& n) const {
    for (int i = (int)scopes.size() - 1; i >= 0; --i) {
      auto it = scopes[i].find(n);
      if (it != scopes[i].end()) {
        return it->second;
      }
    }
    return -1;
  }

  int ensureVar(const std::string& n) {
    int r = lookup(n);
    if (r >= 0) {
      return r;
    }
    return declare(n);
  }

  int newTemp() {  // 임시 레지스터: 그냥 새로 할당
    return f.numRegs++;
  }

  int emit(const Instr& ins) {
    f.code.push_back(ins);
    return (int)f.code.size() - 1;
  }

  void patchRelJump(int at, int rel) {
    auto& ins = f.code[at];
    switch (ins.op) {
      case Op::JUMP:
        ins.a = rel;
        break;
      case Op::JZ:
      case Op::JNZ:
        ins.b = rel;
        break;
      default:
        throw std::runtime_error("patchRelJump on non-jump");
    }
  }

  Op binToOp(const BinaryOp& op) {
    switch (op) {
      case BinaryOp::ADD:
        return Op::ADD;
      case BinaryOp::SUB:
        return Op::SUB;
      case BinaryOp::MUL:
        return Op::MUL;
      case BinaryOp::DIV:
        return Op::DIV;
      case BinaryOp::MOD:
        return Op::MOD;
      case BinaryOp::EQ:
        return Op::EQ;
      case BinaryOp::NEQ:
        return Op::NE;
      case BinaryOp::LT:
        return Op::LT;
      case BinaryOp::LTE:
        return Op::LE;
      case BinaryOp::GT:
        return Op::GT;
      case BinaryOp::GTE:
        return Op::GE;
      default:
        throw std::runtime_error("unknown binary op: " + toString(op));
    }
  }

  Op unToOp(const UnaryOp& op) {
    switch (op) {
      case UnaryOp::UNM:
        return Op::UNM;
      case UnaryOp::POS:
        return Op::POS;
      case UnaryOp::LNOT:
        return Op::LNOT;
    }
  }

  int emitExprTo(const IL::Expression& e, int dst) {
    return std::visit(
        [&](auto const& node) -> int {
          using T = std::decay_t<decltype(node)>;

          if constexpr (std::is_same_v<T, IL::LiteralExpr>) {
            int k = addConst(node.value);
            emit({Op::LOADK, dst, k, 0});
            return dst;
          } else if constexpr (std::is_same_v<T, IL::VariableExpr>) {
            int src = lookup(node.name);
            emit({Op::MOVE, dst, src, 0});
            return dst;
          } else if constexpr (std::is_same_v<T, IL::UnaryExpr>) {
            int r = newTemp();
            emitExprTo(*node.right, r);
            emit({unToOp(node.op), dst, r, 0});
            return dst;
          } else if constexpr (std::is_same_v<T, IL::BinaryExpr>) {
            int L = newTemp();
            emitExprTo(*node.left, L);
            int R = newTemp();
            emitExprTo(*node.right, R);
            emit({binToOp(node.op), dst, L, R});
            return dst;
          } else {
            static_assert(!sizeof(T*), "unhandled IL expr");
          }
        },
        e.node);
  }

  int materialize(const IL::Expression& e) {
    int dst = newTemp();
    emitExprTo(e, dst);
    return dst;
  }

  void emitStmt(const IL::Statement& s,
                const std::unordered_map<std::string, int>& funIndex) {
    std::visit(
        [&](auto const& st) {
          using T = std::decay_t<decltype(st)>;

          if constexpr (std::is_same_v<T, IL::PrintStmt>) {
            int r = materialize(*st.expression);
            emit({Op::PRINT, r, 0, 0});
          } else if constexpr (std::is_same_v<T, IL::BlockStmt>) {
            push();
            for (auto const& sp : st.statements) {
              emitStmt(*sp, funIndex);
            }
            pop();
          } else if constexpr (std::is_same_v<T, IL::VarDeclStmt>) {
            std::ignore = declare(st.name);
          } else if constexpr (std::is_same_v<T, IL::AssignmentStmt>) {
            int dst = ensureVar(st.targetName);
            emitExprTo(*st.value, dst);
          } else if constexpr (std::is_same_v<T, IL::IfStmt>) {
            int c = materialize(*st.condition);
            int jzAt = emit({Op::JZ, c, 0, 0});
            emitStmt(*st.thenBranch, funIndex);
            int jmpEndAt = emit({Op::JUMP, 0, 0, 0});
            int elseIp = (int)f.code.size();
            patchRelJump(jzAt, elseIp - (jzAt + 1));
            if (st.elseBranch) {
              emitStmt(*st.elseBranch, funIndex);
            }
            int endIp = (int)f.code.size();
            patchRelJump(jmpEndAt, endIp - (jmpEndAt + 1));
          } else if constexpr (std::is_same_v<T, IL::WhileStmt>) {
            int loopTop = (int)f.code.size();
            int c = materialize(*st.condition);
            int jzAt = emit({Op::JZ, c, 0, 0});
            emitStmt(*st.body, funIndex);
            std::ignore =
                emit({Op::JUMP, loopTop - ((int)f.code.size() + 1), 0, 0});
            int endIp = (int)f.code.size();
            patchRelJump(jzAt, endIp - (jzAt + 1));
          } else if constexpr (std::is_same_v<T, IL::ReturnStmt>) {
            int r;
            if (st.value) {
              r = materialize(*st.value);
            } else {
              r = newTemp();
              int k = addConst(LiteralValue{});
              emit({Op::LOADK, r, k, 0});
            }
            emit({Op::RET, r, 0, 0});
          } else if constexpr (std::is_same_v<T, IL::CallStmt>) {
            auto it = funIndex.find(st.calleeName);
            if (it == funIndex.end()) {
              throw std::runtime_error("unknown function: " + st.calleeName);
            }

            int argBase = f.numRegs;
            for (size_t i = 0; i < st.arguments.size(); ++i) {
              int r = newTemp();
              emitExprTo(*st.arguments[i], r);
            }
            int dst = -1;
            if (st.targetName) {
              dst = ensureVar(*st.targetName);
            } else {
              dst = newTemp();
            }
            emit({Op::CALL, dst, it->second, argBase});
          } else if constexpr (std::is_same_v<T, IL::FuncDeclStmt>) {
          } else {
            static_assert(!sizeof(T*), "unhandled IL stmt");
          }
        },
        s.node);
  }

  void emitBlock(const IL::BlockStmt& b,
                 const std::unordered_map<std::string, int>& funIndex) {
    push();
    for (auto const& sp : b.statements) {
      emitStmt(*sp, funIndex);
    }
    pop();
  }
};

}  // namespace

Module compileModuleSkeleton(const IL::Program& p) {
  Module m;
  for (auto const& stp : p.statements) {
    if (auto* fd = std::get_if<IL::FuncDeclStmt>(&stp->node)) {
      Function f;
      f.name = fd->name;
      f.numParams = (int)fd->params.size();
      m.functions.push_back(std::move(f));
    }
  }
  Function mainF;
  mainF.name = "$main";
  mainF.numParams = 0;
  m.main = (int)m.functions.size();
  m.functions.push_back(std::move(mainF));
  return m;
}

Module BC::compileFromIL(const IL::Program& p) {
  Module m = compileModuleSkeleton(p);

  std::unordered_map<std::string, int> funIndex;
  for (int i = 0; i < (int)m.functions.size(); ++i) {
    funIndex[m.functions[i].name] = i;
  }

  for (auto const& stp : p.statements) {
    if (auto* fd = std::get_if<IL::FuncDeclStmt>(&stp->node)) {
      FnBuilder fb;
      fb.f.name = fd->name;
      fb.f.numParams = (int)fd->params.size();
      fb.push();
      for (int i = 0; i < fb.f.numParams; ++i) {
        fb.declare(fd->params[i]);
      }
      fb.emitBlock(*fd->body, funIndex);
      if (fb.f.code.empty() || fb.f.code.back().op != Op::RET) {
        int r = fb.newTemp();
        int k = fb.addConst(LiteralValue{});
        fb.emit({Op::LOADK, r, k, 0});
        fb.emit({Op::RET, r, 0, 0});
      }
      m.functions[funIndex[fd->name]] = std::move(fb.f);
    }
  }

  {
    FnBuilder fb;
    fb.f.name = "$main";
    fb.push();
    for (auto const& stp : p.statements) {
      if (std::holds_alternative<IL::FuncDeclStmt>(stp->node)) {
        continue;
      }
      fb.emitStmt(*stp, funIndex);
    }
    fb.emit({Op::HALT, 0, 0, 0});
    m.functions[m.main] = std::move(fb.f);
  }

  return m;
}
