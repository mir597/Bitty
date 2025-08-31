#include "compiler/ConstantPropagation.hpp"

#include <cassert>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

#include "common/IL.hpp"
#include "common/LiteralValue.hpp"  // LiteralValue, op_unary/op_binary, isTruthy

using namespace bitty;

namespace {

// ---------- Lattice value ----------
struct CPValue {
  enum class Tag { Bottom, Const, Top } tag = Tag::Bottom;
  LiteralValue value{};

  BITTY_CP_ALWAYS_INLINE static CPValue bottom() {
    return CPValue{Tag::Bottom, {}};
  }
  BITTY_CP_ALWAYS_INLINE static CPValue top() {
    return CPValue{Tag::Top, {}};
  }
  BITTY_CP_ALWAYS_INLINE static CPValue cst(const LiteralValue& v) {
    return CPValue{Tag::Const, v};
  }

  BITTY_CP_ALWAYS_INLINE bool isBottom() const {
    return tag == Tag::Bottom;
  }
  BITTY_CP_ALWAYS_INLINE bool isConst() const {
    return tag == Tag::Const;
  }
  BITTY_CP_ALWAYS_INLINE bool isTop() const {
    return tag == Tag::Top;
  }
};

BITTY_CP_ALWAYS_INLINE bool litEqual(const LiteralValue& a,
                                     const LiteralValue& b) {
  if (a.index() != b.index()) {
    return false;
  }
  auto vis = [](auto const& x, auto const& y) noexcept -> bool {
    using X = std::decay_t<decltype(x)>;
    using Y = std::decay_t<decltype(y)>;
    if constexpr (std::is_same_v<X, Y>) {
      return x == y;
    } else {
      return false;
    }
  };
  return std::visit(vis, a, b);
}

// Lattice join
BITTY_CP_ALWAYS_INLINE CPValue cpJoin(const CPValue& A, const CPValue& B) {
  if (A.tag == B.tag) {
    if (A.tag != CPValue::Tag::Const) {
      return A;
    }
    return litEqual(A.value, B.value) ? A : CPValue::top();
  }
  if (A.isBottom()) {
    return B;
  }
  if (B.isBottom()) {
    return A;
  }
  return CPValue::top();
}

// ---------- Environment ----------
struct Binding {
  CPValue val = CPValue::bottom();
};

struct Env {
  std::vector<std::unordered_map<std::string, Binding>> frames;

  void push() {
    frames.emplace_back();
  }
  void pop() {
    frames.pop_back();
  }

  Binding* find(const std::string& n) {
    for (int i = int(frames.size()) - 1; i >= 0; --i) {
      auto it = frames[i].find(n);
      if (it != frames[i].end()) {
        return &it->second;
      }
    }
    return nullptr;
  }

  bool hasHere(const std::string& n) const {
    return !frames.empty() && frames.back().count(n) > 0;
  }

  void declare(const std::string& n, Binding b = {}) {
    assert(!frames.empty());
    frames.back()[n] = std::move(b);
  }

  // Set or declare; returns true if binding changed or inserted.
  bool set(const std::string& n, const CPValue& v) {
    if (auto* b = find(n)) {
      if (b->val.tag != v.tag ||
          (b->val.isConst() && !litEqual(b->val.value, v.value))) {
        b->val = v;
        return true;
      }
      return false;
    }
    declare(n, Binding{v});
    return true;
  }

  // Merge using union of keys across frames[i], A.frames[i], B.frames[i].
  bool mergeFrom(const Env& A, const Env& B) {
    bool changed = false;
    assert(frames.size() == A.frames.size() &&
           A.frames.size() == B.frames.size());

    for (size_t i = 0; i < frames.size(); ++i) {
      std::unordered_set<std::string> keys;
      keys.reserve(frames[i].size() + A.frames[i].size() + B.frames[i].size());
      for (auto& kv : frames[i]) {
        keys.insert(kv.first);
      }
      for (auto& kv : A.frames[i]) {
        keys.insert(kv.first);
      }
      for (auto& kv : B.frames[i]) {
        keys.insert(kv.first);
      }

      for (auto const& name : keys) {
        CPValue va = CPValue::bottom(), vb = CPValue::bottom();
        if (auto it = A.frames[i].find(name); it != A.frames[i].end()) {
          va = it->second.val;
        }
        if (auto it = B.frames[i].find(name); it != B.frames[i].end()) {
          vb = it->second.val;
        }

        CPValue nv = cpJoin(va, vb);

        auto itSelf = frames[i].find(name);
        if (itSelf == frames[i].end()) {
          frames[i].emplace(name, Binding{nv});
          changed = true;
        } else {
          const auto& old = itSelf->second.val;
          const bool diff =
              (old.tag != nv.tag) ||
              (old.isConst() && nv.isConst() && !litEqual(old.value, nv.value));
          if (diff) {
            itSelf->second.val = nv;
            changed = true;
          }
        }
      }
    }
    return changed;
  }

  Env clone() const {
    Env e;
    e.frames = frames;
    return e;
  }  // may allocate
};

// ---------- Facts & Context ----------
struct Facts {
  std::unordered_map<const IL::Expression*, CPValue> expr;
};

struct Context {
  Env env;
  Facts* facts = nullptr;  // shared across clones

  Context clone() const {
    Context c;
    c.env = env;
    c.facts = facts;
    return c;
  }
};

// ---------- Module ----------
struct FuncRec {
  IL::FuncDeclStmt* node = nullptr;
  std::vector<CPValue> paramVals;
  CPValue returnVal = CPValue::bottom();
};

struct Module {
  IL::Program* prog = nullptr;
  std::unordered_map<std::string, FuncRec> fun;
};

// Record a fact for an expression; returns true if updated.
bool noteFact(Facts& facts, const IL::Expression* e, const CPValue& v) {
  auto it = facts.expr.find(e);
  if (it == facts.expr.end()) {
    facts.expr.emplace(e, v);
    return true;
  }
  const CPValue& old = it->second;
  if (old.tag != v.tag ||
      (old.isConst() && v.isConst() && !litEqual(old.value, v.value))) {
    it->second = v;
    return true;
  }
  return false;
}

// ---------- Expression analysis (no AST mutation here) ----------
static BITTY_CP_ALWAYS_INLINE CPValue evaluateExprImpl(IL::Expression& e,
                                                       Context& ctx) {
  using namespace IL;
  auto vis = [&](auto& node) -> CPValue {
    using T = std::decay_t<decltype(node)>;

    if constexpr (std::is_same_v<T, LiteralExpr>) {
      return CPValue::cst(node.value);

    } else if constexpr (std::is_same_v<T, VariableExpr>) {
      if (auto* b = ctx.env.find(node.name)) {
        return b->val.isConst()
                   ? b->val
                   : (b->val.isBottom() ? CPValue::bottom() : CPValue::top());
      } else {
        return CPValue::top();
      }

    } else if constexpr (std::is_same_v<T, UnaryExpr>) {
      const CPValue rv = evaluateExprImpl(*node.right, ctx);
      if (rv.isConst()) {
        LiteralValue v = op_unary(node.op, rv.value);
        return CPValue::cst(v);
      } else if (rv.isBottom()) {
        return CPValue::bottom();
      } else {
        return CPValue::top();
      }

    } else if constexpr (std::is_same_v<T, BinaryExpr>) {
      const CPValue lv = evaluateExprImpl(*node.left, ctx);
      const CPValue rv = evaluateExprImpl(*node.right, ctx);
      if (lv.isConst() && rv.isConst()) {
        LiteralValue v = op_binary(lv.value, node.op, rv.value);
        return CPValue::cst(v);
      } else if (lv.isBottom() || rv.isBottom()) {
        return CPValue::bottom();
      } else {
        return CPValue::top();
      }

    } else {
      static_assert(!sizeof(T*), "unhandled expr kind");
    }
  };
  return std::visit(vis, e.node);
}

// Outer wrapper that also records facts.
CPValue evaluateExpr(IL::Expression& e, Context& ctx) {
  assert(ctx.facts && "Context.facts must be set");
  CPValue out = evaluateExprImpl(e, ctx);
  std::ignore = noteFact(*ctx.facts, &e, out);
  return out;
}

// ---------- Forward decls for stmt/block (order matters) ----------
bool propagateStmt(IL::Statement& s, Context& ctx, Module& M, CPValue* retVal);

// Define block first (stmt is forward-declared).
bool propagateBlock(IL::BlockStmt& b, Context& ctx, Module& M,
                    CPValue* retVal) {
  bool changed = false;
  ctx.env.push();
  for (auto& sp : b.statements) {
    changed |= propagateStmt(*sp, ctx, M, retVal);
  }
  ctx.env.pop();
  return changed;
}

// ---------- Statement analysis (no AST mutation here) ----------
bool propagateStmt(IL::Statement& s, Context& ctx, Module& M, CPValue* retVal) {
  using namespace IL;
  bool changed = false;

  std::visit(
      [&](auto& st) {
        using T = std::decay_t<decltype(st)>;

        if constexpr (std::is_same_v<T, PrintStmt>) {
          std::ignore = evaluateExpr(*st.expression, ctx);
        } else if constexpr (std::is_same_v<T, VarDeclStmt>) {
          if (!ctx.env.hasHere(st.name)) {
            ctx.env.declare(st.name, Binding{CPValue::bottom()});
          }
        } else if constexpr (std::is_same_v<T, AssignmentStmt>) {
          CPValue rhs = evaluateExpr(*st.value, ctx);
          std::ignore = ctx.env.set(st.targetName, rhs);
        } else if constexpr (std::is_same_v<T, IfStmt>) {
          CPValue cv = evaluateExpr(*st.condition, ctx);

          Context thenC = ctx.clone();
          Context elseC = ctx.clone();

          bool thenMods = propagateBlock(
              std::get<IL::BlockStmt>(st.thenBranch->node), thenC, M, retVal);
          bool elseMods = false;
          if (st.elseBranch) {
            elseMods = propagateBlock(
                std::get<IL::BlockStmt>(st.elseBranch->node), elseC, M, retVal);
          }
          changed |= (thenMods || elseMods);

          bool merged = false;
          if (cv.isConst()) {
            bool takeThen = isTruthy(cv.value);
            merged = ctx.env.mergeFrom(takeThen ? thenC.env : elseC.env,
                                       takeThen ? thenC.env : elseC.env);
          } else {
            merged = ctx.env.mergeFrom(thenC.env, elseC.env);
          }
          if (merged) {
            changed = true;
          }

        } else if constexpr (std::is_same_v<T, WhileStmt>) {
          // Local fixpoint in loop body
          Context inC = ctx.clone();
          bool iterChanged = true;
          bool factsChangedInLoop = false;
          int iter = 0, kMaxIter = 64;
          while (iterChanged && iter++ < kMaxIter) {
            Context bodyC = inC.clone();
            factsChangedInLoop |= propagateBlock(
                std::get<IL::BlockStmt>(st.body->node), bodyC, M, retVal);

            Context joinedC = inC.clone();
            bool m = joinedC.env.mergeFrom(inC.env, bodyC.env);

            bool eq = true;
            if (!m) {
              if (joinedC.env.frames.size() == inC.env.frames.size()) {
                for (size_t i = 0; i < joinedC.env.frames.size(); ++i) {
                  for (auto const& [name, b] : joinedC.env.frames[i]) {
                    auto it = inC.env.frames[i].find(name);
                    if (it == inC.env.frames[i].end()) {
                      eq = false;
                      break;
                    }
                    const auto& a = it->second.val;
                    const auto& c = b.val;
                    if (a.tag != c.tag ||
                        (a.isConst() && !litEqual(a.value, c.value))) {
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
            } else {
              eq = false;
            }

            inC = std::move(joinedC);
            iterChanged = m || !eq;
          }

          bool merged = ctx.env.mergeFrom(ctx.env, inC.env);
          if (merged) {
            changed = true;
          }

          std::ignore = evaluateExpr(*st.condition,
                                     ctx);  // refresh facts on merged env
          if (factsChangedInLoop) {
            changed = true;
          }

        } else if constexpr (std::is_same_v<T, ReturnStmt>) {
          if (retVal) {
            if (st.value) {
              CPValue rv = evaluateExpr(*st.value, ctx);
              CPValue newRet = cpJoin(*retVal, rv);
              bool diff = (newRet.tag != retVal->tag) ||
                          (newRet.isConst() && retVal->isConst() &&
                           !litEqual(newRet.value, retVal->value));
              if (diff) {
                *retVal = newRet;
                changed = true;
              }
            } else {
              CPValue newRet =
                  cpJoin(*retVal, CPValue::cst(LiteralValue{std::monostate{}}));
              bool diff = (newRet.tag != retVal->tag) ||
                          (newRet.isConst() && retVal->isConst() &&
                           !litEqual(newRet.value, retVal->value));
              if (diff) {
                *retVal = newRet;
                changed = true;
              }
            }
          }

        } else if constexpr (std::is_same_v<T, CallStmt>) {
          std::vector<CPValue> argVals;
          argVals.reserve(st.arguments.size());
          for (auto& a : st.arguments) {
            argVals.push_back(evaluateExpr(*a, ctx));
          }

          if (auto it = M.fun.find(st.calleeName); it != M.fun.end()) {
            auto& fr = it->second;
            if (fr.paramVals.size() < argVals.size()) {
              fr.paramVals.resize(argVals.size(), CPValue::bottom());
            }
            for (size_t i = 0; i < argVals.size(); ++i) {
              CPValue joined = cpJoin(fr.paramVals[i], argVals[i]);
              bool diff = (joined.tag != fr.paramVals[i].tag) ||
                          (joined.isConst() && fr.paramVals[i].isConst() &&
                           !litEqual(joined.value, fr.paramVals[i].value));
              if (diff) {
                fr.paramVals[i] = joined;
                changed = true;
              }
            }
            if (st.targetName) {
              std::ignore = ctx.env.set(*st.targetName, fr.returnVal);
            }
          } else {
            if (st.targetName) {
              std::ignore = ctx.env.set(*st.targetName, CPValue::top());
            }
          }

        } else if constexpr (std::is_same_v<T, FuncDeclStmt>) {
          /* no-op */

        } else if constexpr (std::is_same_v<T, BlockStmt>) {
          changed |= propagateBlock(st, ctx, M, retVal);

        } else {
          static_assert(!sizeof(T*), "unhandled stmt kind");
        }
      },
      s.node);

  return changed;
}

// ---------- AST rewriting (apply recorded facts) ----------
void applyConstants(IL::Expression& e, const Facts& facts) {
  std::visit(
      [&](auto& node) {
        using T = std::decay_t<decltype(node)>;
        if constexpr (std::is_same_v<T, IL::LiteralExpr>) {
          return;
        } else if constexpr (std::is_same_v<T, IL::VariableExpr>) {
          auto it = facts.expr.find(&e);
          if (it != facts.expr.end() && it->second.isConst()) {
            e.node = IL::LiteralExpr{it->second.value};
          }
        } else if constexpr (std::is_same_v<T, IL::UnaryExpr>) {
          applyConstants(*node.right, facts);
          auto it = facts.expr.find(&e);
          if (it != facts.expr.end() && it->second.isConst()) {
            e.node = IL::LiteralExpr{it->second.value};
          }
        } else if constexpr (std::is_same_v<T, IL::BinaryExpr>) {
          applyConstants(*node.left, facts);
          applyConstants(*node.right, facts);
          auto it = facts.expr.find(&e);
          if (it != facts.expr.end() && it->second.isConst()) {
            e.node = IL::LiteralExpr{it->second.value};
          }
        }
      },
      e.node);
}

void applyConstantsStmt(IL::Statement& s, const Facts& facts) {
  std::visit(
      [&](auto& st) {
        using T = std::decay_t<decltype(st)>;
        if constexpr (std::is_same_v<T, IL::PrintStmt>) {
          applyConstants(*st.expression, facts);
        } else if constexpr (std::is_same_v<T, IL::AssignmentStmt>) {
          applyConstants(*st.value, facts);
        } else if constexpr (std::is_same_v<T, IL::IfStmt>) {
          applyConstants(*st.condition, facts);
          for (auto& sp :
               std::get<IL::BlockStmt>(st.thenBranch->node).statements) {
            applyConstantsStmt(*sp, facts);
          }
          if (st.elseBranch) {
            for (auto& sp :
                 std::get<IL::BlockStmt>(st.elseBranch->node).statements) {
              applyConstantsStmt(*sp, facts);
            }
          }
        } else if constexpr (std::is_same_v<T, IL::WhileStmt>) {
          applyConstants(*st.condition, facts);
          for (auto& sp : std::get<IL::BlockStmt>(st.body->node).statements) {
            applyConstantsStmt(*sp, facts);
          }
        } else if constexpr (std::is_same_v<T, IL::ReturnStmt>) {
          if (st.value) {
            applyConstants(*st.value, facts);
          }
        } else if constexpr (std::is_same_v<T, IL::CallStmt>) {
          for (auto& a : st.arguments) {
            applyConstants(*a, facts);
          }
        } else if constexpr (std::is_same_v<T, IL::BlockStmt>) {
          for (auto& sp : st.statements) {
            applyConstantsStmt(*sp, facts);
          }
        } else if constexpr (std::is_same_v<T, IL::FuncDeclStmt>) {
          for (auto& sp : st.body->statements) {
            applyConstantsStmt(*sp, facts);
          }
        }
      },
      s.node);
}

// ---------- Top-level drivers ----------
bool propagateFunction(FuncRec& fr, Module& M, Facts& facts) {
  auto& fn = *fr.node;

  Context ctx;
  ctx.facts = &facts;
  ctx.env.push();

  if (fr.paramVals.size() < fn.params.size()) {
    fr.paramVals.resize(fn.params.size(), CPValue::bottom());
  }
  for (size_t i = 0; i < fn.params.size(); ++i) {
    ctx.env.declare(fn.params[i], Binding{fr.paramVals[i]});
  }

  CPValue newRet = fr.returnVal;
  bool changed = propagateBlock(*fn.body, ctx, M, &newRet);

  if (newRet.tag != fr.returnVal.tag ||
      (newRet.isConst() && fr.returnVal.isConst() &&
       !litEqual(newRet.value, fr.returnVal.value))) {
    fr.returnVal = newRet;
    changed = true;
  }

  return changed;
}

bool propagateTopLevel(IL::Program& p, Module& M, Facts& facts) {
  Context ctx;
  ctx.facts = &facts;
  ctx.env.push();
  bool changed = false;
  for (auto& st : p.statements) {
    changed |= propagateStmt(*st, ctx, M, /*ret*/ nullptr);
  }
  ctx.env.pop();
  return changed;
}

}  // namespace

namespace bitty::IL {

void propagateConstants(Program& program) {
  // Local state (no globals)
  Facts facts;

  Module M;
  M.prog = &program;

  // Collect functions
  for (auto& st : program.statements) {
    if (auto* fd = std::get_if<IL::FuncDeclStmt>(&st->node)) {
      FuncRec rec;
      rec.node = fd;
      rec.paramVals.resize(fd->params.size(), CPValue::bottom());
      rec.returnVal = CPValue::bottom();
      M.fun[fd->name] = std::move(rec);
    }
  }

  // Global fixpoint
  bool changed = true;
  int iter = 0, kMaxIter = 64;
  while (changed && iter++ < kMaxIter) {
    changed = false;
    for (auto& [name, fr] : M.fun) {
      changed |= propagateFunction(fr, M, facts);
    }
    changed |= propagateTopLevel(program, M, facts);
  }

  // Apply constant replacements
  for (auto& st : program.statements) {
    applyConstantsStmt(*st, facts);
  }
}

}  // namespace bitty::IL
