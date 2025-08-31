#include "compiler/ILOptimize.hpp"

#include <algorithm>
#include <functional>
#include <optional>
#include <set>
#include <sstream>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <variant>
#include <vector>

#include "common/LiteralValue.hpp"

// NOTE: This file only uses public IL structs (simple POD-like nodes) so that
// extending the IR later remains straightforward. Helpers are factored out.

namespace bitty::IL {
namespace {

// ---------- utilities ----------

int& gensymCounter() {
  static int c = 0;
  return c;
}

std::string gensym(const std::string& prefix) {
  int id = ++gensymCounter();
  std::ostringstream oss;
  oss << prefix << id;
  return oss.str();
}

// Build a structural key for an expression (no commutativity assumed).
void exprKeyRec(const Expression& e, std::ostringstream& oss) {
  std::visit(
      [&](auto const& node) {
        using T = std::decay_t<decltype(node)>;
        if constexpr (std::is_same_v<T, LiteralExpr>) {
          std::visit(
              [&](auto const& v) {
                using U = std::decay_t<decltype(v)>;
                if constexpr (std::is_same_v<U, std::monostate>) {
                  oss << "K:nil";
                } else if constexpr (std::is_same_v<U, bool>) {
                  oss << "K:b:" << (v ? "1" : "0");
                } else if constexpr (std::is_same_v<U, int64_t>) {
                  oss << "K:i:" << v;
                } else if constexpr (std::is_same_v<U, double>) {
                  oss << "K:d:";
                  std::ostringstream t;
                  t.setf(std::ios::fixed);
                  t.precision(17);
                  t << v;
                  oss << t.str();
                } else if constexpr (std::is_same_v<U, std::string>) {
                  oss << "K:s:" << v;
                }
              },
              node.value);
        } else if constexpr (std::is_same_v<T, VariableExpr>) {
          oss << "V:" << node.name;
        } else if constexpr (std::is_same_v<T, UnaryExpr>) {
          oss << "U:" << toString(node.op) << "(";
          exprKeyRec(*node.right, oss);
          oss << ")";
        } else if constexpr (std::is_same_v<T, BinaryExpr>) {
          oss << "B:" << toString(node.op) << "(";
          exprKeyRec(*node.left, oss);
          oss << ",";
          exprKeyRec(*node.right, oss);
          oss << ")";
        } else {
          static_assert(!sizeof(T*), "Unhandled expr alt");
        }
      },
      e.node);
}

std::string keyOf(const Expression& e) {
  std::ostringstream oss;
  exprKeyRec(e, oss);
  return oss.str();
}

void collectVarsRec(const Expression& e, std::vector<std::string>& out) {
  std::visit(
      [&](auto const& node) {
        using T = std::decay_t<decltype(node)>;
        if constexpr (std::is_same_v<T, VariableExpr>) {
          out.push_back(node.name);
        } else if constexpr (std::is_same_v<T, UnaryExpr>) {
          collectVarsRec(*node.right, out);
        } else if constexpr (std::is_same_v<T, BinaryExpr>) {
          collectVarsRec(*node.left, out);
          collectVarsRec(*node.right, out);
        } else {
          // literals: nothing
        }
      },
      e.node);
}

std::vector<std::string> collectVars(const Expression& e) {
  std::vector<std::string> v;
  collectVarsRec(e, v);
  std::sort(v.begin(), v.end());
  v.erase(std::unique(v.begin(), v.end()), v.end());
  return v;
}

using VersionMap = std::unordered_map<std::string, int>;
using VersionSig = std::vector<std::pair<std::string, int>>;

VersionSig snapshot(const std::vector<std::string>& vars,
                    const VersionMap& vm) {
  VersionSig sig;
  sig.reserve(vars.size());
  for (auto const& n : vars) {
    auto it = vm.find(n);
    int ver = (it == vm.end() ? 0 : it->second);
    sig.push_back({n, ver});
  }
  return sig;
}

bool sameSig(const VersionSig& a, const VersionSig& b) {
  if (a.size() != b.size()) {
    return false;
  }
  for (size_t i = 0; i < a.size(); ++i) {
    if (a[i].first != b[i].first || a[i].second != b[i].second) {
      return false;
    }
  }
  return true;
}

struct Occur {
  int stmtIndex;
  std::string key;
  std::vector<std::string> deps;
  VersionSig sig;
  std::unique_ptr<Expression> exemplar;  // for reconstruction
};

struct ExprInfo {
  std::vector<Occur> occs;
};

// Only consider non-trivial expressions for hoisting (unary/binary).
bool isHoistCandidate(const Expression& e) {
  return std::visit(
      [&](auto const& node) -> bool {
        using T = std::decay_t<decltype(node)>;
        if constexpr (std::is_same_v<T, UnaryExpr>) {
          return true;
        }
        if constexpr (std::is_same_v<T, BinaryExpr>) {
          return true;
        }
        return false;
      },
      e.node);
}

// --------------- Statement builders (lightweight, aggregate init)
// ---------------
std::unique_ptr<Expression> makeVarExpr(const std::string& name,
                                        const Ty::Tag& like) {
  auto e = std::make_unique<Expression>();
  e->type = like;
  VariableExpr v;
  v.name = name;
  e->node = std::move(v);
  return e;
}

std::unique_ptr<Statement> makeVarDecl(const std::string& name) {
  auto st = std::make_unique<Statement>();
  VarDeclStmt vd;
  vd.name = name;
  st->node = std::move(vd);
  return st;
}

std::unique_ptr<Statement> makeAssign(const std::string& target,
                                      std::unique_ptr<Expression> rhs) {
  auto st = std::make_unique<Statement>();
  AssignmentStmt as;
  as.targetName = target;
  as.value = std::move(rhs);
  st->node = std::move(as);
  return st;
}

std::unique_ptr<Expression> makeNilExpr() {
  auto e = std::make_unique<Expression>();
  LiteralExpr lit;
  lit.value = LiteralValue{};
  e->node = std::move(lit);
  return e;
}

// --------------- walkers (non-allocating) ---------------

// Transform expression in-place using preorder callback; if callback returns a
// replacement, use it and do not traverse children.
void transformExpr(
    std::unique_ptr<Expression>& e,
    const std::function<std::unique_ptr<Expression>(const Expression&)>& f) {
  if (!e) {
    return;
  }
  if (auto rep = f(*e)) {
    e = std::move(rep);
    return;
  }
  std::visit(
      [&](auto& node) {
        using T = std::decay_t<decltype(node)>;
        if constexpr (std::is_same_v<T, UnaryExpr>) {
          transformExpr(node.right, f);
        } else if constexpr (std::is_same_v<T, BinaryExpr>) {
          transformExpr(node.left, f);
          transformExpr(node.right, f);
        } else {
          // literals, variables
        }
      },
      e->node);
}

void forEachExpr(Expression& e, const std::function<void(Expression&)>& f) {
  f(e);
  std::visit(
      [&](auto& node) {
        using T = std::decay_t<decltype(node)>;
        if constexpr (std::is_same_v<T, UnaryExpr>) {
          forEachExpr(*node.right, f);
        } else if constexpr (std::is_same_v<T, BinaryExpr>) {
          forEachExpr(*node.left, f);
          forEachExpr(*node.right, f);
        } else {
          // literal/variable
        }
      },
      e.node);
}

// --------------- CSE analysis (per block) ---------------

void scanExpr(const Expression& e, int stmtIndex, VersionMap const& versions,
              std::unordered_map<std::string, ExprInfo>& info) {
  // postorder: ensure we register subexpressions as well
  std::visit(
      [&](auto const& node) {
        using T = std::decay_t<decltype(node)>;
        if constexpr (std::is_same_v<T, UnaryExpr>) {
          scanExpr(*node.right, stmtIndex, versions, info);
        } else if constexpr (std::is_same_v<T, BinaryExpr>) {
          scanExpr(*node.left, stmtIndex, versions, info);
          scanExpr(*node.right, stmtIndex, versions, info);
        }
      },
      e.node);
  if (!isHoistCandidate(e)) {
    return;
  }
  std::string k = keyOf(e);
  auto deps = collectVars(e);
  auto sig = snapshot(deps, versions);
  Occur occ;
  occ.stmtIndex = stmtIndex;
  occ.key = std::move(k);
  occ.deps = std::move(deps);
  occ.sig = std::move(sig);
  occ.exemplar = cloneExpr(e);
  info[occ.key].occs.push_back(std::move(occ));
}

void scanStmt(const Statement& s, int stmtIndex, VersionMap& versions,
              std::unordered_map<std::string, ExprInfo>& info) {
  std::visit(
      [&](auto const& st) {
        using T = std::decay_t<decltype(st)>;
        if constexpr (std::is_same_v<T, PrintStmt>) {
          scanExpr(*st.expression, stmtIndex, versions, info);
        } else if constexpr (std::is_same_v<T, BlockStmt>) {
          // nested blocks handled recursively by caller
        } else if constexpr (std::is_same_v<T, VarDeclStmt>) {
          // nothing
        } else if constexpr (std::is_same_v<T, AssignmentStmt>) {
          scanExpr(*st.value, stmtIndex, versions, info);
          // assignment kills the variable
          versions[st.targetName] += 1;
        } else if constexpr (std::is_same_v<T, IfStmt>) {
          scanExpr(*st.condition, stmtIndex, versions, info);
          // do not scan inside — separate pass per nested block
        } else if constexpr (std::is_same_v<T, WhileStmt>) {
          scanExpr(*st.condition, stmtIndex, versions, info);
        } else if constexpr (std::is_same_v<T, ReturnStmt>) {
          if (st.value) {
            scanExpr(*st.value, stmtIndex, versions, info);
          }
        } else if constexpr (std::is_same_v<T, CallStmt>) {
          for (auto const& arg : st.arguments) {
            scanExpr(*arg, stmtIndex, versions, info);
          }
          if (st.targetName) {
            versions[*st.targetName] += 1;  // CALL writes result into target
          }
        } else if constexpr (std::is_same_v<T, FuncDeclStmt>) {
          // function bodies handled separately by the driver
        } else {
          static_assert(!sizeof(T*), "unhandled stmt");
        }
      },
      s.node);
}

struct HoistPlan {
  // key+sig identifies a particular value instance stable within a range
  std::string key;
  VersionSig sig;
  int atIndex;  // insert point (before this statement index)
  std::string varName;
  std::unique_ptr<Expression> expr;  // pre-cloned exemplar to assign
};

struct SigKeyHash {
  size_t operator()(VersionSig const& s) const noexcept {
    size_t h = 1469598103934665603ull;  // FNV-ish
    for (auto const& [n, v] : s) {
      h ^= std::hash<std::string>{}(n) + 0x9e3779b97f4a7c15ull + (h << 6) +
           (h >> 2);
      h ^= std::hash<int>{}(v) + 0x9e3779b97f4a7c15ull + (h << 6) + (h >> 2);
    }
    return h;
  }
};

// Build plan for a block: choose earliest occurrence j such that from j..end
// version sig is stable.
std::vector<HoistPlan> planHoists(
    const std::unordered_map<std::string, ExprInfo>& info) {
  std::vector<HoistPlan> plans;
  for (auto const& [key, ei] : info) {
    if (ei.occs.size() < 2) {
      continue;
    }
    // try earliest dominating occurrence
    for (size_t j = 0; j < ei.occs.size(); ++j) {
      bool ok = true;
      size_t uses = 0;
      for (size_t k = j; k < ei.occs.size(); ++k) {
        if (!sameSig(ei.occs[j].sig, ei.occs[k].sig)) {
          ok = false;
          break;
        }
        ++uses;
      }
      if (ok && uses >= 2) {
        HoistPlan hp;
        hp.key = key;
        hp.sig = ei.occs[j].sig;
        hp.atIndex = ei.occs[j].stmtIndex;
        hp.varName = gensym("_cse");
        hp.expr = cloneExpr(*ei.occs[j].exemplar);
        plans.push_back(std::move(hp));
        break;  // do not add multiple plans for the same key
      }
    }
  }
  // sort by insertion point ascending so we can insert deterministically
  std::sort(plans.begin(), plans.end(), [](auto const& a, auto const& b) {
    if (a.atIndex != b.atIndex) {
      return a.atIndex < b.atIndex;
    }
    return a.varName < b.varName;
  });
  return plans;
}

// Rewrite one block in-place using the hoist plan.
void cseBlock(BlockStmt& b) {
  if (b.statements.empty()) {
    return;
  }

  // 1) Analysis pass
  VersionMap versions;
  std::unordered_map<std::string, ExprInfo> info;
  info.reserve(b.statements.size() * 4 + 8);

  for (int i = 0; i < (int)b.statements.size(); ++i) {
    scanStmt(*b.statements[i], i, versions, info);
  }

  // 2) Choose hoists
  auto plans = planHoists(info);

  // Index plans by insertion index
  std::unordered_map<int, std::vector<HoistPlan>> byIndex;
  for (auto& hp : plans) {
    byIndex[hp.atIndex].push_back(std::move(hp));
  }

  // Map (key,sig) -> varName for quick substitution lookup
  struct KeySig {
    std::string key;
    VersionSig sig;
  };
  struct KeySigHash {
    size_t operator()(KeySig const& ks) const noexcept {
      size_t h = std::hash<std::string>{}(ks.key);
      h ^= SigKeyHash{}(ks.sig) + 0x9e3779b97f4a7c15ull + (h << 6) + (h >> 2);
      return h;
    }
  };
  struct KeySigEq {
    bool operator()(KeySig const& a, KeySig const& b) const noexcept {
      return a.key == b.key && sameSig(a.sig, b.sig);
    }
  };

  std::unordered_map<KeySig, std::string, KeySigHash, KeySigEq> subst;
  for (auto const& [idx, vec] : byIndex) {
    for (auto const& hp : vec) {
      subst.insert({KeySig{hp.key, hp.sig}, hp.varName});
    }
  }

  // 3) Rewrite with insertions
  VersionMap cur;  // re-run versions with the new stream
  std::vector<std::unique_ptr<Statement>> newStmts;
  newStmts.reserve(b.statements.size() + plans.size() * 2);

  auto rewriteExprAt = [&](std::unique_ptr<Expression>& e) {
    transformExpr(e,
                  [&](const Expression& curE) -> std::unique_ptr<Expression> {
                    if (!isHoistCandidate(curE)) {
                      return nullptr;
                    }
                    std::string k = keyOf(curE);
                    auto deps = collectVars(curE);
                    auto sig = snapshot(deps, cur);
                    auto it = subst.find(KeySig{k, sig});
                    if (it == subst.end()) {
                      return nullptr;
                    }
                    // reuse type from the current expression to preserve
                    // annotation
                    return makeVarExpr(it->second, curE.type);
                  });
  };

  for (int i = 0; i < (int)b.statements.size(); ++i) {
    // Insert hoisted defs scheduled before statement i
    auto pit = byIndex.find(i);
    if (pit != byIndex.end()) {
      for (auto& hp : pit->second) {
        newStmts.push_back(makeVarDecl(hp.varName));
        // use the type of the exemplar to initialize type on the var value
        auto rhs = cloneExpr(*hp.expr);
        newStmts.push_back(makeAssign(hp.varName, std::move(rhs)));
      }
    }

    // Clone + rewrite the original statement
    auto st = cloneStmt(*b.statements[i]);
    std::visit(
        [&](auto& stnode) {
          using T = std::decay_t<decltype(stnode)>;
          if constexpr (std::is_same_v<T, PrintStmt>) {
            rewriteExprAt(stnode.expression);
          } else if constexpr (std::is_same_v<T, BlockStmt>) {
            // recursively optimize nested block
            cseBlock(stnode);
          } else if constexpr (std::is_same_v<T, VarDeclStmt>) {
            // nothing
          } else if constexpr (std::is_same_v<T, AssignmentStmt>) {
            rewriteExprAt(stnode.value);
            cur[stnode.targetName] += 1;
          } else if constexpr (std::is_same_v<T, IfStmt>) {
            rewriteExprAt(stnode.condition);
            // recurse
            if (stnode.thenBranch) {
              if (auto* bptr =
                      std::get_if<BlockStmt>(&stnode.thenBranch->node)) {
                cseBlock(*bptr);
              }
            }
            if (stnode.elseBranch) {
              if (auto* bptr =
                      std::get_if<BlockStmt>(&stnode.elseBranch->node)) {
                cseBlock(*bptr);
              }
            }
          } else if constexpr (std::is_same_v<T, WhileStmt>) {
            rewriteExprAt(stnode.condition);
            if (stnode.body) {
              if (auto* bptr = std::get_if<BlockStmt>(&stnode.body->node)) {
                cseBlock(*bptr);
              }
            }
          } else if constexpr (std::is_same_v<T, ReturnStmt>) {
            if (stnode.value) {
              rewriteExprAt(stnode.value);
            }
          } else if constexpr (std::is_same_v<T, CallStmt>) {
            for (auto& arg : stnode.arguments) {
              rewriteExprAt(arg);
            }
            if (stnode.targetName) {
              cur[*stnode.targetName] += 1;
            }
          } else if constexpr (std::is_same_v<T, FuncDeclStmt>) {
            // function bodies optimized by driver; keep declaration as-is
          } else {
            static_assert(!sizeof(T*), "unhandled stmt variant");
          }
        },
        st->node);

    newStmts.push_back(std::move(st));
  }

  // Append hoists scheduled at the end (if any)
  int endIdx = (int)b.statements.size();
  auto pitEnd = byIndex.find(endIdx);
  if (pitEnd != byIndex.end()) {
    for (auto& hp : pitEnd->second) {
      newStmts.push_back(makeVarDecl(hp.varName));
      auto rhs = cloneExpr(*hp.expr);
      newStmts.push_back(makeAssign(hp.varName, std::move(rhs)));
    }
  }

  b.statements = std::move(newStmts);
}

// --------------- Inlining single-use functions ---------------

struct FnRef {
  // index in top-level Program.statements
  int topIndex;
  FuncDeclStmt* fn;  // owned by Program via Statement
};

void collectFunctions(Program& p, std::unordered_map<std::string, FnRef>& out) {
  out.clear();
  for (int i = 0; i < (int)p.statements.size(); ++i) {
    if (auto* fd = std::get_if<FuncDeclStmt>(&p.statements[i]->node)) {
      out.emplace(fd->name, FnRef{i, fd});
    }
  }
}

void walkStmt(const Statement& s,
              const std::function<void(const CallStmt&)>& onCall) {
  std::visit(
      [&](auto const& st) {
        using T = std::decay_t<decltype(st)>;
        if constexpr (std::is_same_v<T, PrintStmt>) {
          (void)st;
        } else if constexpr (std::is_same_v<T, BlockStmt>) {
          for (auto const& sp : st.statements) {
            walkStmt(*sp, onCall);
          }
        } else if constexpr (std::is_same_v<T, VarDeclStmt>) {
        } else if constexpr (std::is_same_v<T, AssignmentStmt>) {
        } else if constexpr (std::is_same_v<T, IfStmt>) {
          walkStmt(*st.thenBranch, onCall);
          if (st.elseBranch) {
            walkStmt(*st.elseBranch, onCall);
          }
        } else if constexpr (std::is_same_v<T, WhileStmt>) {
          walkStmt(*st.body, onCall);
        } else if constexpr (std::is_same_v<T, ReturnStmt>) {
        } else if constexpr (std::is_same_v<T, CallStmt>) {
          onCall(st);
        } else if constexpr (std::is_same_v<T, FuncDeclStmt>) {
          // dive into body
          for (auto const& sp : st.body->statements) {
            walkStmt(*sp, onCall);
          }
        } else {
          static_assert(!sizeof(T*), "walkStmt missing alt");
        }
      },
      s.node);
}

std::unordered_map<std::string, int> countCalls(const Program& p) {
  std::unordered_map<std::string, int> cnt;
  for (auto const& sp : p.statements) {
    walkStmt(*sp, [&](const CallStmt& c) { cnt[c.calleeName] += 1; });
  }
  return cnt;
}

bool hasRecursiveCall(const FuncDeclStmt& f) {
  bool rec = false;
  for (auto const& sp : f.body->statements) {
    walkStmt(*sp, [&](const CallStmt& c) {
      if (c.calleeName == f.name) {
        rec = true;
      }
    });
    if (rec) {
      break;
    }
  }
  return rec;
}

int countReturns(const Statement& s) {
  int cnt = 0;
  std::visit(
      [&](auto const& st) {
        using T = std::decay_t<decltype(st)>;
        if constexpr (std::is_same_v<T, BlockStmt>) {
          for (auto const& sp : st.statements) {
            cnt += countReturns(*sp);
          }
        } else if constexpr (std::is_same_v<T, IfStmt>) {
          cnt += countReturns(*st.thenBranch);
          if (st.elseBranch) {
            cnt += countReturns(*st.elseBranch);
          }
        } else if constexpr (std::is_same_v<T, WhileStmt>) {
          cnt += countReturns(*st.body);
        } else if constexpr (std::is_same_v<T, ReturnStmt>) {
          cnt += 1;
        } else if constexpr (std::is_same_v<T, FuncDeclStmt>) {
          // ignore
        } else {
          // other statements: 0
        }
      },
      s.node);
  return cnt;
}

bool singleReturnAtTail(const FuncDeclStmt& f) {
  // exactly one return and it's the last statement in body
  if (f.body->statements.empty()) {
    return false;
  }
  int total = 0;
  for (auto const& sp : f.body->statements) {
    total += countReturns(*sp);
  }
  if (total != 1) {
    return false;
  }
  if (!std::holds_alternative<ReturnStmt>(f.body->statements.back()->node)) {
    return false;
  }
  return true;
}

// rename variables in a statement tree according to provided mapping
void renameVarsStmt(Statement& s,
                    const std::unordered_map<std::string, std::string>& m) {
  auto renameExpr = [&](std::unique_ptr<Expression>& e) {
    transformExpr(e,
                  [&](const Expression& curE) -> std::unique_ptr<Expression> {
                    if (auto* vx = std::get_if<VariableExpr>(&curE.node)) {
                      auto it = m.find(vx->name);
                      if (it != m.end()) {
                        return makeVarExpr(it->second, curE.type);
                      }
                    }
                    return nullptr;
                  });
  };

  std::visit(
      [&](auto& st) {
        using T = std::decay_t<decltype(st)>;
        if constexpr (std::is_same_v<T, PrintStmt>) {
          renameExpr(st.expression);
        } else if constexpr (std::is_same_v<T, BlockStmt>) {
          for (auto& sp : st.statements) {
            renameVarsStmt(*sp, m);
          }
        } else if constexpr (std::is_same_v<T, VarDeclStmt>) {
          auto it = m.find(st.name);
          if (it != m.end()) {
            st.name = it->second;
          }
        } else if constexpr (std::is_same_v<T, AssignmentStmt>) {
          renameExpr(st.value);
          auto it = m.find(st.targetName);
          if (it != m.end()) {
            st.targetName = it->second;
          }
        } else if constexpr (std::is_same_v<T, IfStmt>) {
          renameExpr(st.condition);
          renameVarsStmt(*st.thenBranch, m);
          if (st.elseBranch) {
            renameVarsStmt(*st.elseBranch, m);
          }
        } else if constexpr (std::is_same_v<T, WhileStmt>) {
          renameExpr(st.condition);
          renameVarsStmt(*st.body, m);
        } else if constexpr (std::is_same_v<T, ReturnStmt>) {
          if (st.value) {
            renameExpr(st.value);
          }
        } else if constexpr (std::is_same_v<T, CallStmt>) {
          for (auto& arg : st.arguments) {
            renameExpr(arg);
          }
        } else if constexpr (std::is_same_v<T, FuncDeclStmt>) {
          // shouldn't be nested here
        }
      },
      s.node);
}

void collectLocalDecls(const Statement& s, std::vector<std::string>& out) {
  std::visit(
      [&](auto const& st) {
        using T = std::decay_t<decltype(st)>;
        if constexpr (std::is_same_v<T, BlockStmt>) {
          for (auto const& sp : st.statements) {
            collectLocalDecls(*sp, out);
          }
        } else if constexpr (std::is_same_v<T, VarDeclStmt>) {
          out.push_back(st.name);
        } else if constexpr (std::is_same_v<T, IfStmt>) {
          collectLocalDecls(*st.thenBranch, out);
          if (st.elseBranch) {
            collectLocalDecls(*st.elseBranch, out);
          }
        } else if constexpr (std::is_same_v<T, WhileStmt>) {
          collectLocalDecls(*st.body, out);
        } else {
          // ignore
        }
      },
      s.node);
}

// Replace a single CallStmt (at [block, idx]) with the inlined body.
// Preconditions: callee has single return at tail, non-recursive.
void inlineAt(BlockStmt& block, int idx, const CallStmt& call,
              const FuncDeclStmt& callee) {
  // 1) Construct fresh names for params and locals
  std::unordered_map<std::string, std::string> ren;
  for (size_t i = 0; i < callee.params.size(); ++i) {
    ren[callee.params[i]] = gensym("_inl_p");
  }
  std::vector<std::string> locals;
  {
    // collect locals from callee.body
    Statement wrapper;
    wrapper.node = BlockStmt{};
    auto& BW = std::get<BlockStmt>(wrapper.node);
    auto bodyClone = cloneBlock(*callee.body);
    BW.statements = std::move(bodyClone->statements);
    collectLocalDecls(wrapper, locals);
  }
  for (auto const& n : locals) {
    if (ren.find(n) == ren.end()) {
      ren[n] = gensym("_inl_v");
    }
  }

  // 2) Clone body and apply renaming (wrap as a Statement to reuse renamer)
  auto bodyClone = cloneBlock(*callee.body);
  Statement bodyWrapper;
  bodyWrapper.node = BlockStmt{};
  {
    auto& BW = std::get<BlockStmt>(bodyWrapper.node);
    BW.statements = std::move(bodyClone->statements);
  }
  renameVarsStmt(bodyWrapper, ren);
  auto& RENBody = std::get<BlockStmt>(bodyWrapper.node);

  // 3) Prepare arguments copies into param variables
  std::vector<std::unique_ptr<Statement>> injected;
  injected.reserve(callee.params.size() * 2 + RENBody.statements.size() + 2);
  for (size_t i = 0; i < callee.params.size(); ++i) {
    auto const& pFresh = ren[callee.params[i]];
    injected.push_back(makeVarDecl(pFresh));
    if (i < call.arguments.size()) {
      injected.push_back(makeAssign(pFresh, cloneExpr(*call.arguments[i])));
    } else {
      injected.push_back(makeAssign(pFresh, makeNilExpr()));
    }
  }

  // 4) Extract final return
  std::unique_ptr<Expression> retExpr;
  if (!RENBody.statements.empty() &&
      std::holds_alternative<ReturnStmt>(RENBody.statements.back()->node)) {
    auto& rs = std::get<ReturnStmt>(RENBody.statements.back()->node);
    if (rs.value) {
      retExpr = cloneExpr(*rs.value);
    }
    // remove the terminal return
    RENBody.statements.pop_back();
  }

  // 5) Splice: injected + body + (store into target)
  for (auto& sp : RENBody.statements) {
    injected.push_back(std::move(sp));
  }
  if (call.targetName && retExpr) {
    injected.push_back(makeAssign(*call.targetName, std::move(retExpr)));
  } else if (call.targetName && !retExpr) {
    // function returns void/nil: assign nil to keep semantics consistent
    injected.push_back(makeAssign(*call.targetName, makeNilExpr()));
  }

  // 6) Replace in block
  auto& vec = block.statements;
  // erase call
  vec.erase(vec.begin() + idx);
  // insert injected at idx
  vec.insert(vec.begin() + idx, std::make_move_iterator(injected.begin()),
             std::make_move_iterator(injected.end()));
}

// Find & inline the first occurrence in a block (recursively).
bool inlineInBlock(BlockStmt& b, const std::string& name,
                   const FuncDeclStmt& callee) {
  for (int i = 0; i < (int)b.statements.size(); ++i) {
    if (auto* cs = std::get_if<CallStmt>(&b.statements[i]->node)) {
      if (cs->calleeName == name) {
        inlineAt(b, i, *cs, callee);
        return true;
      }
    } else if (auto* is = std::get_if<IfStmt>(&b.statements[i]->node)) {
      if (auto* tb = std::get_if<BlockStmt>(&is->thenBranch->node)) {
        if (inlineInBlock(*tb, name, callee)) {
          return true;
        }
      }
      if (is->elseBranch) {
        if (auto* eb = std::get_if<BlockStmt>(&is->elseBranch->node)) {
          if (inlineInBlock(*eb, name, callee)) {
            return true;
          }
        }
      }
    } else if (auto* ws = std::get_if<WhileStmt>(&b.statements[i]->node)) {
      if (auto* bb = std::get_if<BlockStmt>(&ws->body->node)) {
        if (inlineInBlock(*bb, name, callee)) {
          return true;
        }
      }
    }
  }
  return false;
}

// Inline one occurrence anywhere in the program; returns true if something
// changed.
bool inlineOne(Program& p) {
  std::unordered_map<std::string, FnRef> fns;
  collectFunctions(p, fns);

  auto callCounts = countCalls(p);
  for (auto const& [name, ref] : fns) {
    if (callCounts[name] != 1) {
      continue;
    }
    if (hasRecursiveCall(*ref.fn)) {
      continue;
    }
    if (!singleReturnAtTail(*ref.fn)) {
      continue;
    }

    // Search each toplevel statement in order and inline first match we can
    // edit.
    bool done = false;
    for (int i = 0; i < (int)p.statements.size() && !done; ++i) {
      auto& st = p.statements[i];
      if (auto* cs = std::get_if<CallStmt>(&st->node)) {
        if (cs->calleeName != name) {
          continue;
        }
        // Replace this single statement by a BlockStmt to inject code
        BlockStmt host;
        host.statements.push_back(std::move(p.statements[i]));
        inlineAt(host, 0, *cs, *ref.fn);
        p.statements.erase(p.statements.begin() + i);
        p.statements.insert(p.statements.begin() + i,
                            std::make_move_iterator(host.statements.begin()),
                            std::make_move_iterator(host.statements.end()));
        done = true;
        break;
      } else if (auto* fs = std::get_if<FuncDeclStmt>(&st->node)) {
        auto* body = fs->body.get();
        if (inlineInBlock(*body, name, *ref.fn)) {
          done = true;
          break;
        }
      } else if (auto* bs = std::get_if<BlockStmt>(&st->node)) {
        if (inlineInBlock(*bs, name, *ref.fn)) {
          done = true;
          break;
        }
      }
    }

    if (!done) {
      continue;
    }

    // Remove the function declaration from the toplevel.
    for (int i = 0; i < (int)p.statements.size(); ++i) {
      if (auto* fd = std::get_if<FuncDeclStmt>(&p.statements[i]->node)) {
        if (fd->name == name) {
          p.statements.erase(p.statements.begin() + i);
          break;
        }
      }
    }
    return true;  // inlined one
  }
  return false;
}

void runInlining(Program& p) {
  // Keep inlining until fixed point (bounded by number of functions)
  int guard = 1000;
  while (guard-- > 0) {
    if (!inlineOne(p)) {
      break;
    }
  }
}

// --------------- Driver ---------------

void cseProgram(Program& p) {
  // Optimize top-level non-declaration segments ($main) in place, and function
  // bodies.
  int i = 0;
  while (i < (int)p.statements.size()) {
    if (std::holds_alternative<FuncDeclStmt>(p.statements[i]->node)) {
      if (auto* fd = std::get_if<FuncDeclStmt>(&p.statements[i]->node)) {
        cseBlock(*fd->body);
      }
      ++i;
      continue;
    }
    // collect a contiguous segment of non-decls [i, j)
    int j = i;
    BlockStmt seg;
    while (j < (int)p.statements.size() &&
           !std::holds_alternative<FuncDeclStmt>(p.statements[j]->node)) {
      seg.statements.push_back(std::move(p.statements[j]));
      ++j;
    }
    // optimize the segment as a block
    cseBlock(seg);
    // splice back
    p.statements.erase(p.statements.begin() + i, p.statements.begin() + j);
    p.statements.insert(p.statements.begin() + i,
                        std::make_move_iterator(seg.statements.begin()),
                        std::make_move_iterator(seg.statements.end()));
    i += (int)seg.statements.size();
  }
}

}  // namespace

void optimizeProgram(Program& p) {
  // Phase 1: inline single-use simple functions
  runInlining(p);
  // Phase 2: DAG/CSE per block
  cseProgram(p);
}

}  // namespace bitty::IL