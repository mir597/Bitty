#include "core/TypeInference.hpp"

#include <algorithm>
#include <cassert>
#include <functional>
#include <iostream>
#include <optional>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "core/Common.hpp"
#include "core/IL.hpp"
#include "core/LiteralValue.hpp"
#include "core/Type.hpp"

using namespace bitty;

namespace {

using TTag = Ty::Tag;

inline bool isInt(TTag t) {
  return t == TTag::Int;
}
inline bool isFloat(TTag t) {
  return t == TTag::Float;
}
inline bool isString(TTag t) {
  return t == TTag::String;
}
inline bool isNumber(TTag t) {
  return t == TTag::Int || t == TTag::Float || t == TTag::Number;
}

inline TTag numericJoin(TTag a, TTag b) {
  // Join two *numeric* tags to the most precise numeric result.
  if (isFloat(a) || isFloat(b)) {
    return TTag::Float;
  } else if (isInt(a) && isInt(b)) {
    return TTag::Int;
  } else if (isNumber(a) || isNumber(b)) {
    return TTag::Number;
  }
  return TTag::Number;  // fallback for soundness
}

inline TTag tjoin(TTag a, TTag b) {
  return Ty::join(a, b);
}

inline TTag literalType(const LiteralValue& v) {
  return std::visit(
      [](auto const& x) -> TTag {
        using V = std::decay_t<decltype(x)>;
        if constexpr (std::is_same_v<V, std::monostate>) {
          return TTag::Nil;
        } else if constexpr (std::is_same_v<V, bool>) {
          return TTag::Bool;
        } else if constexpr (std::is_same_v<V, int64_t>) {
          return TTag::Int;
        } else if constexpr (std::is_same_v<V, double>) {
          return TTag::Float;
        } else if constexpr (std::is_same_v<V, std::string>) {
          return TTag::String;
        } else {
          return TTag::Unknown;
        }
      },
      v);
}

inline TTag unaryResult(const UnaryOp& op, TTag r) {
  if (op == UnaryOp::LNOT) {
    return TTag::Bool;
  }
  if (op == UnaryOp::UNM || op == UnaryOp::POS) {
    if (isNumber(r)) {
      return r;
    }
    return TTag::Unknown;
  }
  assert(false && "unknown operation");
  unreachable();
}

inline bool isArith(const BinaryOp& op) {
  return (op == BinaryOp::ADD || op == BinaryOp::SUB || op == BinaryOp::MUL ||
          op == BinaryOp::DIV || op == BinaryOp::MOD);
}
inline bool isEq(const BinaryOp& op) {
  return (op == BinaryOp::EQ || op == BinaryOp::NEQ);
}
inline bool isCmp(const BinaryOp& op) {
  return (op == BinaryOp::LT || op == BinaryOp::LTE || op == BinaryOp::GT ||
          op == BinaryOp::GTE);
}
inline bool isLogic(const BinaryOp& op) {
  return (op == BinaryOp::AND || op == BinaryOp::OR);
}

inline TTag binaryResult(TTag l, const BinaryOp& op, TTag r) {
  if (isEq(op) || isCmp(op)) {
    return TTag::Bool;
  } else if (isLogic(op)) {
    return TTag::Bool;
  } else if (isArith(op)) {
    switch (op) {
      case BinaryOp::ADD:
        if (isString(l) || isString(r)) {
          return TTag::String;  // concat
        } else if (isNumber(l) && isNumber(r)) {
          return numericJoin(l, r);
        }
        return TTag::Unknown;
      case BinaryOp::MUL:
        if ((isString(l) && isNumber(r)) || (isString(r) && isNumber(l))) {
          return TTag::String;  // repeat
        } else if (isNumber(l) && isNumber(r)) {
          return numericJoin(l, r);
        }
        return TTag::Unknown;
      case BinaryOp::DIV:
        if (isNumber(l) && isNumber(r)) {
          return TTag::Float;  // runtime: double
        }
        return TTag::Unknown;
      case BinaryOp::SUB:
        if (isNumber(l) && isNumber(r)) {
          return numericJoin(l, r);
        }
        return TTag::Unknown;
      case BinaryOp::MOD:
        if (isNumber(l) && isNumber(r)) {
          return TTag::Int;  // runtime: int
        }
        return TTag::Unknown;
      default:
        assert(false);
    }
  }
  unreachable();
}

struct Binding {
  TTag type = TTag::Unknown;
  std::function<void(TTag)> onCommit;
};

struct Env {
  std::vector<std::unordered_map<std::string, Binding>> frames;
  bool commit = true;

  void push() {
    frames.emplace_back();
  }
  void pop() {
    frames.pop_back();
  }
  bool has(const std::string& n) const {
    for (int i = (int)frames.size() - 1; i >= 0; --i) {
      if (frames[i].find(n) != frames[i].end()) {
        return true;
      }
    }
    return false;
  }
  Binding* find(const std::string& n) {
    for (int i = (int)frames.size() - 1; i >= 0; --i) {
      auto it = frames[i].find(n);
      if (it != frames[i].end()) {
        return &it->second;
      }
    }
    return nullptr;
  }
  void declare(const std::string& n, Binding b) {
    assert(!frames.empty());
    frames.back()[n] = std::move(b);
  }
  bool update(const std::string& n, TTag t) {
    if (auto* b = find(n)) {
      TTag nt = tjoin(b->type, t);
      if (nt != b->type) {
        b->type = nt;
        if (commit && b->onCommit) {
          b->onCommit(nt);
        }
        return true;
      }
    }
    return false;
  }

  Env clone(bool commitFlag) const {
    Env e;
    e.frames = frames;
    e.commit = commitFlag;
    return e;
  }

  bool mergeFrom(const Env& A, const Env& B) {
    bool changed = false;
    assert(frames.size() == A.frames.size() &&
           A.frames.size() == B.frames.size());
    for (size_t i = 0; i < frames.size(); ++i) {
      for (auto& [name, bind] : frames[i]) {
        TTag ta = bind.type;
        if (auto it = A.frames[i].find(name); it != A.frames[i].end()) {
          ta = it->second.type;
        }
        TTag tb = bind.type;
        if (auto it = B.frames[i].find(name); it != B.frames[i].end()) {
          tb = it->second.type;
        }
        TTag nt = tjoin(ta, tb);
        if (nt != bind.type) {
          bind.type = nt;
          if (commit && bind.onCommit) {
            bind.onCommit(nt);
          }
          changed = true;
        }
      }
    }
    return changed;
  }
};

struct RetScan {
  bool hasRet = false, hasVoidRet = false, mustRet = false;
};

RetScan scanBlock(const IL::BlockStmt& b);
RetScan scanStmt(const IL::Statement& s);

RetScan scanBlock(const IL::BlockStmt& b) {
  RetScan out{};
  for (auto const& sp : b.statements) {
    RetScan i = scanStmt(*sp);
    out.hasRet |= i.hasRet;
    out.hasVoidRet |= i.hasVoidRet;
    if (i.mustRet) {
      out.mustRet = true;
      break;
    }
  }
  return out;
}

RetScan scanStmt(const IL::Statement& s) {
  return std::visit(
      [&](auto const& st) -> RetScan {
        using T = std::decay_t<decltype(st)>;
        if constexpr (std::is_same_v<T, IL::ReturnStmt>) {
          return RetScan{true, !st.value, true};
        } else if constexpr (std::is_same_v<T, IL::IfStmt>) {
          auto a = scanBlock(std::get<IL::BlockStmt>(st.thenBranch->node));
          RetScan b{};
          if (st.elseBranch) {
            b = scanBlock(std::get<IL::BlockStmt>(st.elseBranch->node));
          }
          return RetScan{a.hasRet || b.hasRet, a.hasVoidRet || b.hasVoidRet,
                         (st.elseBranch && a.mustRet && b.mustRet)};
        } else if constexpr (std::is_same_v<T, IL::WhileStmt>) {
          auto bi = scanBlock(std::get<IL::BlockStmt>(st.body->node));
          return RetScan{bi.hasRet, bi.hasVoidRet, false};
        } else if constexpr (std::is_same_v<T, IL::BlockStmt>) {
          return scanBlock(st);
        } else {
          return RetScan{};
        }
      },
      s.node);
}

struct FuncRec {
  IL::FuncDeclStmt* node = nullptr;
  std::unordered_map<std::string, int> paramIndexByName;
};

struct Module {
  IL::Program* prog = nullptr;
  std::unordered_map<std::string, FuncRec> fun;
};

bool inferExpr(IL::Expression& e, Env& env);
bool inferStmt(IL::Statement& s, Env& env, Module& M,
               TTag* retType /*nullable*/);
bool inferBlock(IL::BlockStmt& b, Env& env, Module& M,
                TTag* retType /*nullable*/);

bool setTypeIfWider(TTag& slot, TTag cand) {
  TTag nt = tjoin(slot, cand);
  if (nt != slot) {
    slot = nt;
    return true;
  }
  return false;
}

bool inferExpr(IL::Expression& e, Env& env) {
  using namespace IL;
  TTag out = TTag::Unknown;

  std::visit(
      [&](auto& node) {
        using T = std::decay_t<decltype(node)>;
        if constexpr (std::is_same_v<T, LiteralExpr>) {
          out = literalType(node.value);
        } else if constexpr (std::is_same_v<T, VariableExpr>) {
          if (auto* b = env.find(node.name)) {
            out = b->type;
          } else {
            out = TTag::Unknown;
          }
        } else if constexpr (std::is_same_v<T, UnaryExpr>) {
          std::ignore = inferExpr(*node.right, env);
          out = unaryResult(node.op, node.right->type);
        } else if constexpr (std::is_same_v<T, BinaryExpr>) {
          std::ignore = inferExpr(*node.left, env);
          std::ignore = inferExpr(*node.right, env);
          out = binaryResult(node.left->type, node.op, node.right->type);
        } else {
          static_assert(!sizeof(T*), "unhandled expr kind");
        }
      },
      e.node);

  return setTypeIfWider(e.type, out);
}

bool inferStmt(IL::Statement& s, Env& env, Module& M, TTag* retType) {
  using namespace IL;
  bool changed = false;

  std::visit(
      [&](auto& st) {
        using T = std::decay_t<decltype(st)>;

        if constexpr (std::is_same_v<T, PrintStmt>) {
          changed |= inferExpr(*st.expression, env);
        } else if constexpr (std::is_same_v<T, VarDeclStmt>) {
          Binding b;
          b.type = s.type;
          b.onCommit = [&s](TTag t) { s.type = t; };
          env.declare(st.name, std::move(b));
        } else if constexpr (std::is_same_v<T, AssignmentStmt>) {
          changed |= inferExpr(*st.value, env);
          if (env.has(st.targetName)) {
            bool u = env.update(st.targetName, st.value->type);
            if (env.commit) {
              changed |= u;
            }
          } else {
            Binding b;
            b.type = st.value->type;
            b.onCommit = [](TTag) {};
            env.declare(st.targetName, std::move(b));
            if (env.commit) {
              changed = true;
            }
          }
        } else if constexpr (std::is_same_v<T, IfStmt>) {
          bool condChanged = inferExpr(*st.condition, env);

          Env envThen = env.clone(false);
          Env envElse = env.clone(false);
          std::ignore = inferBlock(std::get<BlockStmt>(st.thenBranch->node),
                                   envThen, M, retType);
          if (st.elseBranch) {
            std::ignore = inferBlock(std::get<BlockStmt>(st.elseBranch->node),
                                     envElse, M, retType);
          }
          bool merged = env.mergeFrom(envThen, envElse);
          changed |= (condChanged || merged);
        } else if constexpr (std::is_same_v<T, WhileStmt>) {
          changed |= inferExpr(*st.condition, env);
          Env in = env.clone(false);
          Env out = in;
          bool iterChanged = true;
          int iter = 0;
          const int kMaxIter = 64;
          while (iterChanged && iter++ < kMaxIter) {
            Env bodyEnv = in.clone(false);
            iterChanged = inferBlock(std::get<BlockStmt>(st.body->node),
                                     bodyEnv, M, retType);

            Env joined = in.clone(false);
            iterChanged |= joined.mergeFrom(in, bodyEnv);

            bool eq = true;
            if (joined.frames.size() == in.frames.size()) {
              for (size_t i = 0; i < joined.frames.size(); ++i) {
                for (auto const& [name, b] : joined.frames[i]) {
                  auto it = in.frames[i].find(name);
                  if (it == in.frames[i].end() || it->second.type != b.type) {
                    eq = false;
                    break;
                  }
                }
                if (!eq) {
                  break;
                }
              }
            } else {
              eq = false;
            }
            in = std::move(joined);
            iterChanged = iterChanged || !eq;
          }
          changed |= env.mergeFrom(env, in);
        } else if constexpr (std::is_same_v<T, ReturnStmt>) {
          if (st.value) {
            changed |= inferExpr(*st.value, env);
            if (retType) {
              TTag nt = tjoin(*retType, st.value->type);
              if (nt != *retType) {
                *retType = nt;
                changed = true;
              }
            }
          } else {
            if (retType) {
              TTag nt = tjoin(*retType, TTag::Nil);
              if (nt != *retType) {
                *retType = nt;
                changed = true;
              }
            }
          }
        } else if constexpr (std::is_same_v<T, CallStmt>) {
          for (auto& arg : st.arguments) {
            changed |= inferExpr(*arg, env);
          }
          auto it = M.fun.find(st.calleeName);
          if (it != M.fun.end()) {
            auto* fn = it->second.node;
            const size_t nArgs = st.arguments.size();
            if (fn->paramTypes.size() < nArgs) {
              fn->paramTypes.resize(nArgs, TTag::Unknown);
            }
            for (size_t i = 0; i < nArgs; ++i) {
              TTag old = fn->paramTypes[i];
              TTag nt = tjoin(old, st.arguments[i]->type);
              if (nt != old) {
                fn->paramTypes[i] = nt;
                changed = true;
              }
            }
            if (st.targetName) {
              if (env.has(*st.targetName)) {
                bool u = env.update(*st.targetName, fn->returnType);
                if (env.commit) {
                  changed |= u;
                }
              } else {
                Binding b;
                b.type = fn->returnType;
                b.onCommit = [](TTag) {};
                env.declare(*st.targetName, std::move(b));
                if (env.commit) {
                  changed = true;
                }
              }
            }
          } else {
            if (st.targetName) {
              if (env.has(*st.targetName)) {
                changed |= env.update(*st.targetName, TTag::Unknown);
              } else {
                Binding b;
                b.type = TTag::Unknown;
                b.onCommit = [](TTag) {};
                env.declare(*st.targetName, std::move(b));
                changed = true;
              }
            }
          }
        } else if constexpr (std::is_same_v<T, FuncDeclStmt>) {
          /* do nothing */
        } else if constexpr (std::is_same_v<T, BlockStmt>) {
          changed |= inferBlock(st, env, M, retType);
        } else {
          static_assert(!sizeof(T*), "unhandled stmt kind");
        }
      },
      s.node);

  return changed;
}

bool inferBlock(IL::BlockStmt& b, Env& env, Module& M, TTag* retType) {
  bool changed = false;
  env.push();
  for (auto& sp : b.statements) {
    changed |= inferStmt(*sp, env, M, retType);
  }
  env.pop();
  return changed;
}

bool inferFunction(IL::FuncDeclStmt& fn, Module& M) {
  Env env;
  env.commit = true;
  env.push();

  if (fn.paramTypes.size() < fn.params.size()) {
    fn.paramTypes.resize(fn.params.size(), TTag::Unknown);
  }

  for (size_t i = 0; i < fn.params.size(); ++i) {
    Binding b;
    b.type = fn.paramTypes[i];
    b.onCommit = [&fn, i](TTag t) { fn.paramTypes[i] = t; };
    env.declare(fn.params[i], std::move(b));
  }

  TTag oldRet = fn.returnType;
  auto ri = scanBlock(*fn.body);
  bool needsNil = ri.hasVoidRet || !ri.mustRet;
  if (needsNil) {
    fn.returnType = tjoin(fn.returnType, TTag::Nil);
  }

  bool bodyChanged = inferBlock(*fn.body, env, M, &fn.returnType);
  return bodyChanged || (fn.returnType != oldRet);
}

bool inferTopLevel(IL::Program& p, Module& M) {
  Env env;
  env.commit = true;
  env.push();

  bool changed = false;
  for (auto& st : p.statements) {
    changed |= inferStmt(*st, env, M, /*retType*/ nullptr);
  }
  env.pop();
  return changed;
}

}  // namespace

namespace bitty::IL {

void inferTypes(Program& program) {
  Module M;
  M.prog = &program;

  for (auto& stp : program.statements) {
    if (auto* fd = std::get_if<IL::FuncDeclStmt>(&stp->node)) {
      FuncRec r;
      r.node = fd;
      for (size_t i = 0; i < fd->params.size(); ++i) {
        r.paramIndexByName[fd->params[i]] = (int)i;
      }
      M.fun[fd->name] = std::move(r);
    }
  }

  bool changed = true;
  int iter = 0;
  const int kMaxIter = 64;
  while (changed && iter++ < kMaxIter) {
    changed = false;

    for (auto& [name, rec] : M.fun) {
      changed |= inferFunction(*rec.node, M);
    }
    changed |= inferTopLevel(program, M);
  }
}

}  // namespace bitty::IL
