#include "core/Lowering.hpp"

#include <sstream>
#include <stdexcept>
#include <string>
#include <unordered_set>
#include <vector>

#include "core/Common.hpp"
#include "core/LiteralValue.hpp"

using namespace bitty;

namespace {
template <class... Ts>
struct overloaded : Ts... {
  using Ts::operator()...;
};
template <class... Ts>
overloaded(Ts...) -> overloaded<Ts...>;

using ASTExpr = AST::Expression;
using ASTStmt = AST::Statement;
using ILExpr = IL::Expression;
using ILStmt = IL::Statement;

struct Ctx {
  std::vector<IL::BlockStmt*> blockStack;
  int tempCounter = 0;
  std::vector<std::unordered_set<std::string>> nameScopes;
  void pushScope() {
    nameScopes.emplace_back();
  }
  void popScope() {
    nameScopes.pop_back();
  }
  bool used(const std::string& n) const {
    for (int i = (int)nameScopes.size() - 1; i >= 0; --i) {
      if (nameScopes[i].count(n)) {
        return true;
      }
    }
    return false;
  }
  void noteVar(const std::string& n) {
    if (nameScopes.empty()) {
      pushScope();
    }
    nameScopes.back().insert(n);
  }

  std::string newTemp() {
    if (nameScopes.empty()) {
      pushScope();
    }
    for (;;) {
      std::string cand = "__t" + std::to_string(tempCounter++);
      if (!used(cand)) {
        noteVar(cand);
        return cand;
      }
    }
  }

  Ctx() {
    pushScope();
  }

  IL::BlockStmt& cur() {
    if (blockStack.empty()) {
      throw std::runtime_error("lowering: no active block");
    }
    return *blockStack.back();
  }

  void emit(std::unique_ptr<ILStmt> s) {
    if (!s) {
      throw std::runtime_error("emit(nullptr)");
    }
    cur().statements.push_back(std::move(s));
  }

  static bool isLeaf(const IL::Expression& e) {
    return std::holds_alternative<IL::LiteralExpr>(e.node) ||
           std::holds_alternative<IL::VariableExpr>(e.node);
  }

  static std::unique_ptr<IL::Expression> makeVar(std::string name) {
    return create<IL::Expression>(IL::VariableExpr{std::move(name)});
  }

  static std::unique_ptr<IL::Expression> makeLit(const LiteralValue& v) {
    return create<IL::Expression>(IL::LiteralExpr{v});
  }

  static std::unique_ptr<IL::Expression> cloneExpr(const IL::Expression& e) {
    return std::visit(
        overloaded{
            [&](const IL::LiteralExpr& n) { return makeLit(n.value); },
            [&](const IL::VariableExpr& n) { return makeVar(n.name); },
            [&](const IL::UnaryExpr& n) {
              IL::UnaryExpr u;
              u.op = n.op;
              u.right = cloneExpr(*n.right);
              return create<IL::Expression>(std::move(u));
            },
            [&](const IL::BinaryExpr& n) {
              IL::BinaryExpr b;
              b.left = cloneExpr(*n.left);
              b.op = n.op;
              b.right = cloneExpr(*n.right);
              return create<IL::Expression>(std::move(b));
            },
        },
        e.node);
  }

  std::unique_ptr<IL::Expression> ensureLeaf(const AST::Expression& e) {
    if (auto const* call = std::get_if<AST::CallExpr>(&e.node)) {
      auto const* calleeVar =
          std::get_if<AST::VariableExpr>(&call->callee->node);
      if (!calleeVar) {
        throw std::runtime_error("call callee must be identifier");
      }

      std::vector<std::unique_ptr<IL::Expression>> args;
      args.reserve(call->arguments.size());
      for (auto const& a : call->arguments) {
        args.push_back(ensureLeaf(*a));
      }

      // T = f(args)
      std::string t = newTemp();
      emit(create<ILStmt>(IL::VarDeclStmt{t}));

      IL::CallStmt cs;
      cs.targetName = t;
      cs.calleeName = calleeVar->name;
      cs.arguments = std::move(args);
      auto st = std::make_unique<IL::Statement>();
      st->node = std::move(cs);
      emit(std::move(st));

      return makeVar(std::move(t));
    }

    auto one = ensureRValue(*this, e);
    if (isLeaf(*one)) {
      return one;
    }

    std::string t = newTemp();
    emit(create<ILStmt>(IL::VarDeclStmt{t}));

    IL::AssignmentStmt asn;
    asn.targetName = t;
    asn.value = cloneExpr(*one);

    auto st = std::make_unique<IL::Statement>();
    st->node = std::move(asn);
    emit(std::move(st));

    return makeVar(std::move(t));
  }

  static bool isAtom(const IL::Expression& e) {
    return std::holds_alternative<IL::LiteralExpr>(e.node) ||
           std::holds_alternative<IL::VariableExpr>(e.node);
  }

  static std::unique_ptr<IL::Expression> cloneSimpleExpr(
      const AST::Expression& e) {
    return std::visit(
        [&](auto const& node) -> std::unique_ptr<IL::Expression> {
          using T = std::decay_t<decltype(node)>;

          if constexpr (std::is_same_v<T, AST::LiteralExpr>) {
            return create<IL::Expression>(IL::LiteralExpr{node.value});
          } else if constexpr (std::is_same_v<T, AST::VariableExpr>) {
            return create<IL::Expression>(IL::VariableExpr{node.name});
          } else if constexpr (std::is_same_v<T, AST::UnaryExpr>) {
            auto rhs = cloneSimpleExpr(*node.right);
            return create<IL::Expression>(
                IL::UnaryExpr{node.op, std::move(rhs)});
          } else if constexpr (std::is_same_v<T, AST::BinaryExpr>) {
            auto l = cloneSimpleExpr(*node.left);
            auto r = cloneSimpleExpr(*node.right);
            return create<IL::Expression>(
                IL::BinaryExpr{std::move(l), node.op, std::move(r)});
          } else if constexpr (std::is_same_v<T, AST::AssignmentExpr>) {
            throw std::runtime_error("cloneSimple expects no assignment");
          } else if constexpr (std::is_same_v<T, AST::CallExpr>) {
            throw std::runtime_error("cloneSimple expects no call");
          } else {
            static_assert(!sizeof(T*), "unhandled AST expr in cloneSimpleExpr");
          }
        },
        e.node);
  }

  static std::unique_ptr<IL::Expression> cloneILExpr(const IL::Expression& e) {
    return std::visit(
        [&](auto const& node) -> std::unique_ptr<IL::Expression> {
          using T = std::decay_t<decltype(node)>;

          if constexpr (std::is_same_v<T, IL::LiteralExpr>) {
            return create<IL::Expression>(IL::LiteralExpr{node.value});
          } else if constexpr (std::is_same_v<T, IL::VariableExpr>) {
            return create<IL::Expression>(IL::VariableExpr{node.name});
          } else if constexpr (std::is_same_v<T, IL::UnaryExpr>) {
            auto rhs = cloneILExpr(*node.right);
            return create<IL::Expression>(
                IL::UnaryExpr{node.op, std::move(rhs)});
          } else if constexpr (std::is_same_v<T, IL::BinaryExpr>) {
            auto l = cloneILExpr(*node.left);
            auto r = cloneILExpr(*node.right);
            return create<IL::Expression>(
                IL::BinaryExpr{std::move(l), node.op, std::move(r)});
          } else {
            static_assert(!sizeof(T*), "unhandled IL expr in cloneILExpr");
          }
        },
        e.node);
  }

  static std::unique_ptr<IL::Expression> ensureAtom(Ctx& ctx,
                                                    const AST::Expression& e) {
    auto v = ensureRValue(ctx, e);
    if (isAtom(*v)) {
      return v;
    }

    const std::string t = ctx.newTemp();
    ctx.emit(create<IL::Statement>(IL::AssignmentStmt{t, std::move(v)}));
    return create<IL::Expression>(IL::VariableExpr{t});
  }

  static std::unique_ptr<IL::Expression> ensureRValue(
      Ctx& ctx, const AST::Expression& e,
      std::optional<std::string> preferTarget = std::nullopt) {
    return std::visit(
        [&](auto const& node) -> std::unique_ptr<IL::Expression> {
          using T = std::decay_t<decltype(node)>;

          if constexpr (std::is_same_v<T, AST::CallExpr>) {
            const std::string calleeName = ensureCalleeName(ctx, *node.callee);

            std::vector<std::unique_ptr<IL::Expression>> args;
            args.reserve(node.arguments.size());
            for (auto const& a : node.arguments) {
              args.push_back(ensureRValue(ctx, *a));
            }

            std::string tgt;
            if (preferTarget) {
              tgt = *preferTarget;
            } else {
              tgt = ctx.newTemp();
            }

            IL::CallStmt cs;
            cs.targetName = tgt;
            cs.calleeName = calleeName;
            cs.arguments = std::move(args);
            ctx.emit(create<IL::Statement>(std::move(cs)));
            return create<IL::Expression>(IL::VariableExpr{tgt});
          } else if constexpr (std::is_same_v<T, AST::AssignmentExpr>) {
            struct Chain {
              std::vector<std::string> names;
              AST::Expression const* value = nullptr;
            };

            std::function<void(AST::Expression const&, Chain&)> collect =
                [&](AST::Expression const& ex, Chain& ch) {
                  if (auto* a = std::get_if<AST::AssignmentExpr>(&ex.node)) {
                    auto* lhsVar =
                        std::get_if<AST::VariableExpr>(&a->target->node);
                    if (!lhsVar) {
                      throw std::runtime_error(
                          "assignment target must be a variable");
                    }
                    ch.names.push_back(lhsVar->name);
                    collect(*a->value, ch);
                  } else {
                    ch.value = &ex;
                  }
                };

            Chain chain;
            collect(e, chain);
            if (!chain.value) {
              throw std::runtime_error("malformed assignment chain");
            }

            const bool rhsIsCall =
                std::holds_alternative<AST::CallExpr>(chain.value->node);
            std::optional<std::string> lastTarget;
            if (!chain.names.empty()) {
              lastTarget = chain.names.back();
            }
            auto val = ensureRValue(ctx, *chain.value,
                                    rhsIsCall ? lastTarget : std::nullopt);

            int iStart = (int)chain.names.size() - 1;
            if (rhsIsCall) {
              iStart--;
            }
            for (int i = iStart; i >= 0; --i) {
              auto st = std::make_unique<IL::Statement>();
              st->node = IL::AssignmentStmt{chain.names[i], cloneILExpr(*val)};
              ctx.emit(std::move(st));
            }
            return cloneILExpr(*val);
          } else if constexpr (std::is_same_v<T, AST::UnaryExpr>) {
            auto rhsAtom = ensureAtom(ctx, *node.right);

            auto expr = create<IL::Expression>(
                IL::UnaryExpr{node.op, std::move(rhsAtom)});

            if (preferTarget) {
              ctx.emit(create<IL::Statement>(
                  IL::AssignmentStmt{*preferTarget, std::move(expr)}));
              return create<IL::Expression>(IL::VariableExpr{*preferTarget});
            }
            return expr;
          } else if constexpr (std::is_same_v<T, AST::BinaryExpr>) {
            if (node.op == BinaryOp::AND || node.op == BinaryOp::OR) {
              const std::string t =
                  preferTarget ? *preferTarget : ctx.newTemp();
              if (!preferTarget) {
                ctx.emit(create<IL::Statement>(IL::VarDeclStmt{t}));
              }
              auto L = ensureAtom(ctx, *node.left);
              ctx.emit(create<IL::Statement>(IL::AssignmentStmt{
                  t, create<IL::Expression>(IL::LiteralExpr{
                         node.op == BinaryOp::AND ? LiteralValue{false}
                                                  : LiteralValue{true}})}));

              IL::ExpressionNode cond;
              if (node.op == BinaryOp::AND) {
                // Simply ensures the result is a boolean. But !!L is not
                // necessary in this language.
                cond = IL::UnaryExpr{UnaryOp::LNOT,
                                     create<IL::Expression>(IL::UnaryExpr{
                                         UnaryOp::LNOT, std::move(L)})};  // !!L
              } else {  // "||": if (!L) then evaluate R
                cond = IL::UnaryExpr{UnaryOp::LNOT, std::move(L)};
              }
              // then: t = !!R
              IL::BlockStmt thenB;
              {
                auto Rr = ensureAtom(ctx, *node.right);
                auto notR = create<IL::Expression>(
                    IL::UnaryExpr{UnaryOp::LNOT, std::move(Rr)});
                auto b2 = create<IL::Expression>(
                    IL::UnaryExpr{UnaryOp::LNOT, std::move(notR)});  // !!R
                thenB.statements.push_back(create<IL::Statement>(
                    IL::AssignmentStmt{t, std::move(b2)}));
              }
              // if (cond) { thenB; }
              IL::IfStmt ilIf;
              ilIf.condition = create<IL::Expression>(std::move(cond));
              ilIf.thenBranch = create<IL::Statement>(std::move(thenB));
              auto st = std::make_unique<IL::Statement>();
              st->node = std::move(ilIf);
              ctx.emit(std::move(st));
              return create<IL::Expression>(IL::VariableExpr{t});
            } else {
              auto L = ensureAtom(ctx, *node.left);
              auto R = ensureAtom(ctx, *node.right);
              auto expr = create<IL::Expression>(
                  IL::BinaryExpr{std::move(L), node.op, std::move(R)});
              if (preferTarget) {
                ctx.emit(create<IL::Statement>(
                    IL::AssignmentStmt{*preferTarget, std::move(expr)}));
                return create<IL::Expression>(IL::VariableExpr{*preferTarget});
              }
              return expr;
            }
          } else if constexpr (std::is_same_v<T, AST::LiteralExpr>) {
            if (preferTarget) {
              auto out = create<IL::Expression>(IL::LiteralExpr{node.value});
              ctx.emit(create<IL::Statement>(
                  IL::AssignmentStmt{*preferTarget, cloneILExpr(*out)}));
              return create<IL::Expression>(IL::VariableExpr{*preferTarget});
            }
            return create<IL::Expression>(IL::LiteralExpr{node.value});
          } else if constexpr (std::is_same_v<T, AST::VariableExpr>) {
            if (preferTarget && node.name != *preferTarget) {
              auto out = create<IL::Expression>(IL::VariableExpr{node.name});
              ctx.emit(create<IL::Statement>(
                  IL::AssignmentStmt{*preferTarget, cloneILExpr(*out)}));
              return create<IL::Expression>(IL::VariableExpr{*preferTarget});
            }
            return create<IL::Expression>(IL::VariableExpr{node.name});
          }
        },
        e.node);
  }

  static std::unique_ptr<ILExpr> cloneSimple(const ILExpr& e) {
    auto out = std::make_unique<ILExpr>();
    if (auto* v = std::get_if<IL::VariableExpr>(&e.node)) {
      out->node = *v;
    } else if (auto* l = std::get_if<IL::LiteralExpr>(&e.node)) {
      out->node = *l;
    } else {
      throw std::runtime_error("cloneSimple expects literal/variable");
    }
    return out;
  }

  static std::string ensureCalleeName(Ctx& ctx, const AST::Expression& callee) {
    if (auto* v = std::get_if<AST::VariableExpr>(&callee.node)) {
      return v->name;
    }

    const std::string t = ctx.newTemp();

    auto v = ensureRValue(ctx, callee, /*preferTarget=*/t);

    bool already = false;
    if (auto* vv = std::get_if<IL::VariableExpr>(&v->node)) {
      if (vv->name == t) {
        already = true;
      }
    }
    if (!already) {
      ctx.emit(create<IL::Statement>(IL::AssignmentStmt{t, std::move(v)}));
    }
    return t;
  }

  void lowerStmt(const ASTStmt& s);
  std::unique_ptr<IL::BlockStmt> lowerBlock(const AST::BlockStmt& b);
  std::unique_ptr<IL::Statement> lowerBlockAsStmt(const AST::BlockStmt& b);
};

std::unique_ptr<IL::BlockStmt> Ctx::lowerBlock(const AST::BlockStmt& b) {
  auto blk = std::make_unique<IL::BlockStmt>();
  blockStack.push_back(blk.get());
  for (auto& sp : b.statements) {
    lowerStmt(*sp);
  }
  blockStack.pop_back();
  return blk;
}

std::unique_ptr<IL::Statement> Ctx::lowerBlockAsStmt(const AST::BlockStmt& b) {
  auto blk = lowerBlock(b);
  return create<IL::Statement>(std::move(*blk));
}

static bool hasCallExpr(const AST::Expression& e) {
  bool found = false;
  std::function<void(const AST::Expression&)> walk = [&](auto const& ex) {
    std::visit(overloaded{
                   [&](const AST::LiteralExpr&) {},
                   [&](const AST::VariableExpr&) {},
                   [&](const AST::UnaryExpr& u) { walk(*u.right); },
                   [&](const AST::BinaryExpr& b) {
                     walk(*b.left);
                     walk(*b.right);
                   },
                   [&](const AST::AssignmentExpr& a) {
                     walk(*a.target);
                     walk(*a.value);
                   },
                   [&](const AST::CallExpr&) { found = true; },
               },
               ex.node);
  };
  walk(e);
  return found;
}

static std::unique_ptr<IL::Expression> lowerPureNoHoist(
    const AST::Expression& e) {
  return std::visit(
      overloaded{
          [&](const AST::LiteralExpr& l) {
            return create<IL::Expression>(IL::LiteralExpr{l.value});
          },
          [&](const AST::VariableExpr& v) {
            return create<IL::Expression>(IL::VariableExpr{v.name});
          },
          [&](const AST::UnaryExpr& u) {
            auto r = lowerPureNoHoist(*u.right);
            return create<IL::Expression>(IL::UnaryExpr{u.op, std::move(r)});
          },
          [&](const AST::BinaryExpr& b) {
            auto l = lowerPureNoHoist(*b.left);
            auto r = lowerPureNoHoist(*b.right);
            return create<IL::Expression>(
                IL::BinaryExpr{std::move(l), b.op, std::move(r)});
          },
          [&](const AST::AssignmentExpr&) -> std::unique_ptr<IL::Expression> {
            throw std::runtime_error(
                "assignment not allowed in pure condition");
          },
          [&](const AST::CallExpr&) -> std::unique_ptr<IL::Expression> {
            throw std::runtime_error("call not allowed in pure condition");
          },
      },
      e.node);
}

void Ctx::lowerStmt(const ASTStmt& s) {
  std::visit(
      [&](auto const& node) {
        using T = std::decay_t<decltype(node)>;

        if constexpr (std::is_same_v<T, AST::ExpressionStmt>) {
          if (auto* call = std::get_if<AST::CallExpr>(&node.expression->node)) {
            const std::string callee = ensureCalleeName(*this, *call->callee);
            std::vector<std::unique_ptr<IL::Expression>> args;
            args.reserve(call->arguments.size());
            for (auto const& a : call->arguments) {
              args.push_back(ensureRValue(*this, *a));
            }
            IL::CallStmt cs;
            cs.calleeName = callee;
            cs.arguments = std::move(args);
            auto out = std::make_unique<IL::Statement>();
            out->node = std::move(cs);
            emit(std::move(out));
            return;
          }
          std::ignore = ensureRValue(*this, *node.expression);
          return;
        } else if constexpr (std::is_same_v<T, AST::PrintStmt>) {
          auto v = ensureRValue(*this, *node.expression);
          emit(create<IL::Statement>(IL::PrintStmt{std::move(v)}));
        } else if constexpr (std::is_same_v<T, AST::BlockStmt>) {
          emit(create<IL::Statement>(std::move(*lowerBlock(node))));
        } else if constexpr (std::is_same_v<T, AST::VarDeclStmt>) {
          emit(create<IL::Statement>(IL::VarDeclStmt{node.name}));
          if (node.initializer) {
            auto rv = ensureRValue(*this, *node.initializer);
            IL::AssignmentStmt as{node.name, std::move(rv)};
            emit(create<IL::Statement>(std::move(as)));
          }
        } else if constexpr (std::is_same_v<T, AST::IfStmt>) {
          auto cond = ensureRValue(*this, *node.condition);
          IL::IfStmt il;
          il.condition = std::move(cond);
          // then
          if (auto* blk = std::get_if<AST::BlockStmt>(&node.thenBranch->node)) {
            il.thenBranch = lowerBlockAsStmt(*blk);
          } else {
            AST::BlockStmt wrap;
            wrap.statements.push_back(
                std::make_unique<AST::Statement>(std::move(*node.thenBranch)));
            il.thenBranch = lowerBlockAsStmt(wrap);
          }
          // else
          if (node.elseBranch) {
            if (auto* blk =
                    std::get_if<AST::BlockStmt>(&node.elseBranch->node)) {
              il.elseBranch = lowerBlockAsStmt(*blk);
            } else {
              AST::BlockStmt wrap;
              wrap.statements.push_back(std::make_unique<AST::Statement>(
                  std::move(*node.elseBranch)));
              il.elseBranch = lowerBlockAsStmt(wrap);
            }
          }
          auto st = std::make_unique<ILStmt>();
          st->node = std::move(il);
          emit(std::move(st));
        } else if constexpr (std::is_same_v<T, AST::WhileStmt>) {
          if (!hasCallExpr(*node.condition)) {
            IL::WhileStmt il;
            il.condition = lowerPureNoHoist(*node.condition);

            if (auto* blk = std::get_if<AST::BlockStmt>(&node.body->node)) {
              il.body = lowerBlockAsStmt(*blk);
            } else {
              AST::BlockStmt wrap;
              wrap.statements.push_back(
                  std::make_unique<AST::Statement>(std::move(*node.body)));
              il.body = lowerBlockAsStmt(wrap);
            }

            auto st = std::make_unique<ILStmt>();
            st->node = std::move(il);
            emit(std::move(st));
            return;
          }

          const std::string t = newTemp();
          {
            auto rv = ensureRValue(*this, *node.condition);
            auto asn = std::make_unique<ILStmt>();
            asn->node = IL::AssignmentStmt{t, std::move(rv)};
            emit(std::move(asn));
          }

          std::unique_ptr<ILStmt> bodyStmt;
          if (auto* blk = std::get_if<AST::BlockStmt>(&node.body->node)) {
            bodyStmt = lowerBlockAsStmt(*blk);
          } else {
            AST::BlockStmt wrap;
            wrap.statements.push_back(
                std::make_unique<AST::Statement>(std::move(*node.body)));
            bodyStmt = lowerBlockAsStmt(wrap);
          }
          auto* bodyBlk = std::get_if<IL::BlockStmt>(&bodyStmt->node);
          if (!bodyBlk) {
            IL::BlockStmt wrap;
            wrap.statements.push_back(std::move(bodyStmt));
            auto stmt = std::make_unique<ILStmt>();
            stmt->node = std::move(wrap);
            bodyStmt = std::move(stmt);
            bodyBlk = &std::get<IL::BlockStmt>(bodyStmt->node);
          }

          {
            blockStack.push_back(bodyBlk);
            auto rv2 = ensureRValue(*this, *node.condition);
            emit(create<ILStmt>(IL::AssignmentStmt{t, std::move(rv2)}));
            blockStack.pop_back();
          }

          IL::WhileStmt il;
          il.condition = create<IL::Expression>(IL::VariableExpr{t});
          il.body = std::move(bodyStmt);

          auto st = std::make_unique<ILStmt>();
          st->node = std::move(il);
          emit(std::move(st));
        } else if constexpr (std::is_same_v<T, AST::ReturnStmt>) {
          IL::ReturnStmt il;
          if (node.value) {
            il.value = ensureRValue(*this, *node.value);
          }
          auto st = std::make_unique<ILStmt>();
          st->node = std::move(il);
          emit(std::move(st));
        } else if constexpr (std::is_same_v<T, AST::FuncDeclStmt>) {
          IL::FuncDeclStmt f;
          f.name = node.name;
          f.params = node.params;
          pushScope();
          for (auto const& p : f.params) {
            noteVar(p);
          }
          f.body = lowerBlock(*node.body);
          popScope();
          auto st = std::make_unique<ILStmt>();
          st->node = std::move(f);
          emit(std::move(st));
        } else {
          static_assert(!sizeof(T*), "unhandled AST stmt");
        }
      },
      s.node);
}

}  // namespace

namespace bitty::IL {

Program lowerFromAST(const AST::Program& ast) {
  Ctx ctx;
  auto root = std::make_unique<IL::BlockStmt>();
  ctx.blockStack.push_back(root.get());
  for (auto& sp : ast.statements) {
    ctx.lowerStmt(*sp);
  }
  ctx.blockStack.pop_back();

  Program p;
  for (auto& st : root->statements) {
    p.statements.push_back(std::move(st));
  }
  return p;
}

}  // namespace bitty::IL
