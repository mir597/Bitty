#include "interpreter/Interpreter.hpp"

#include <cmath>
#include <cstdint>
#include <functional>
#include <memory>
#include <optional>
#include <sstream>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <variant>

#include "common/LiteralValue.hpp"

using namespace bitty;
using Value = LiteralValue;

namespace {
struct Env;
struct Function {
  std::string name;
  std::vector<std::string> params;
  const AST::BlockStmt* body = nullptr;
  std::shared_ptr<Env> closure;
};

struct Env : std::enable_shared_from_this<Env> {
  std::unordered_map<std::string, Value> vars;
  std::unordered_map<std::string, std::shared_ptr<Function>> funs;
  std::shared_ptr<Env> parent;

  explicit Env(std::shared_ptr<Env> p = nullptr) : parent(std::move(p)) {
  }

  bool hasLocal(const std::string& n) const {
    return vars.find(n) != vars.end();
  }
  Value* findVar(const std::string& n) {
    for (auto e = this; e; e = e->parent.get()) {
      if (auto it = e->vars.find(n); it != e->vars.end()) {
        return &it->second;
      }
    }
    return nullptr;
  }
  std::shared_ptr<Function> findFun(const std::string& n) {
    for (auto e = this; e; e = e->parent.get()) {
      if (auto it = e->funs.find(n); it != e->funs.end()) {
        return it->second;
      }
    }
    return nullptr;
  }
};

struct ReturnSignal {
  Value value;
};

struct VM {
  ExecResult result;
  std::shared_ptr<Env> env;

  explicit VM() : env(std::make_shared<Env>()) {
  }

  struct ScopeGuard {
    VM& vm;
    explicit ScopeGuard(VM& v) : vm(v) {
      vm.env = std::make_shared<Env>(vm.env);
    }
    ~ScopeGuard() {
      vm.env = vm.env->parent;
    }
  };

  void declareVar(const std::string& name, Value init = std::monostate{}) {
    vmCheck(!env->hasLocal(name), "redeclaration: " + name);
    env->vars[name] = std::move(init);
  }

  void declareFun(const std::string& name, const AST::FuncDeclStmt& fd) {
    auto f = std::make_shared<Function>();
    f->name = name;
    f->params = fd.params;
    f->body = fd.body.get();
    f->closure = env;
    env->funs[name] = std::move(f);
  }

  [[noreturn]] void vmFail(const std::string& msg) {
    throw std::runtime_error(msg);
  }
  void vmCheck(bool cond, const std::string& msg) {
    if (!cond) {
      vmFail(msg);
    }
  }

  Value eval(const AST::Expression& e) {
    return std::visit(
        [&](auto const& node) -> Value {
          using T = std::decay_t<decltype(node)>;

          if constexpr (std::is_same_v<T, AST::LiteralExpr>) {
            return node.value;
          } else if constexpr (std::is_same_v<T, AST::VariableExpr>) {
            if (auto* pv = env->findVar(node.name)) {
              return *pv;
            }
            vmFail("undefined variable: " + node.name);
          } else if constexpr (std::is_same_v<T, AST::UnaryExpr>) {
            Value r = eval(*node.right);
            return op_unary(node.op, r);
          } else if constexpr (std::is_same_v<T, AST::BinaryExpr>) {
            // short circuit
            if (node.op == BinaryOp::AND) {
              Value l = eval(*node.left);
              if (!isTruthy(l)) {
                return LiteralValue{false};
              }
              Value r = eval(*node.right);
              return LiteralValue{isTruthy(r)};
            } else if (node.op == BinaryOp::OR) {
              Value l = eval(*node.left);
              if (isTruthy(l)) {
                return LiteralValue{true};
              }
              Value r = eval(*node.right);
              return LiteralValue{isTruthy(r)};
            } else {
              Value l = eval(*node.left);
              Value r = eval(*node.right);
              return op_binary(l, node.op, r);
            }
          } else if constexpr (std::is_same_v<T, AST::AssignmentExpr>) {
            auto* lhsVar = std::get_if<AST::VariableExpr>(&node.target->node);
            vmCheck(lhsVar != nullptr, "assignment target must be identifier");
            Value v = eval(*node.value);
            Value* slot = env->findVar(lhsVar->name);
            vmCheck(slot != nullptr,
                    "assign to undeclared variable: " + lhsVar->name);
            *slot = v;
            return v;
          } else if constexpr (std::is_same_v<T, AST::CallExpr>) {
            auto* calleeVar =
                std::get_if<AST::VariableExpr>(&node.callee->node);
            vmCheck(calleeVar != nullptr, "call callee must be identifier");
            auto fn = env->findFun(calleeVar->name);
            vmCheck(fn != nullptr, "undefined function: " + calleeVar->name);
            vmCheck(fn->params.size() == node.arguments.size(),
                    "arity mismatch on call: " + calleeVar->name);

            auto callEnv = std::make_shared<Env>(fn->closure);
            for (size_t i = 0; i < fn->params.size(); ++i) {
              Value arg = eval(*node.arguments[i]);
              callEnv->vars[fn->params[i]] = std::move(arg);
            }

            VM callee;
            callee.result.stdout_text = result.stdout_text;
            callee.env = callEnv;
            try {
              execBlockInherited(callee, *fn->body);
              result.stdout_text = callee.result.stdout_text;
              return Value{std::monostate{}};
            } catch (const ReturnSignal& rs) {
              result.stdout_text = callee.result.stdout_text;
              return rs.value;
            }
          } else {
            static_assert(!sizeof(T*), "unhandled expression node");
          }
        },
        e.node);
  }

  void exec(const AST::Statement& s) {
    std::visit(
        [&](auto const& st) {
          using T = std::decay_t<decltype(st)>;

          if constexpr (std::is_same_v<T, AST::ExpressionStmt>) {
            std::ignore = eval(*st.expression);
          } else if constexpr (std::is_same_v<T, AST::PrintStmt>) {
            Value v = eval(*st.expression);
            result.stdout_text += valueToString(v);
            result.stdout_text.push_back('\n');
          } else if constexpr (std::is_same_v<T, AST::BlockStmt>) {
            ScopeGuard guard(*this);
            for (auto const& sp : st.statements) {
              exec(*sp);
            }
          } else if constexpr (std::is_same_v<T, AST::VarDeclStmt>) {
            if (st.initializer) {
              declareVar(st.name, eval(*st.initializer));
            } else {
              declareVar(st.name, std::monostate{});
            }
          } else if constexpr (std::is_same_v<T, AST::IfStmt>) {
            Value c = eval(*st.condition);
            if (isTruthy(c)) {
              exec(*st.thenBranch);
            } else if (st.elseBranch) {
              exec(*st.elseBranch);
            }
          } else if constexpr (std::is_same_v<T, AST::WhileStmt>) {
            while (isTruthy(eval(*st.condition))) {
              exec(*st.body);
            }
          } else if constexpr (std::is_same_v<T, AST::ReturnStmt>) {
            if (st.value) {
              throw ReturnSignal{eval(*st.value)};
            } else {
              throw ReturnSignal{std::monostate{}};
            }
          } else if constexpr (std::is_same_v<T, AST::FuncDeclStmt>) {
            declareFun(st.name, st);
          } else {
            static_assert(!sizeof(T*), "unhandled statement node");
          }
        },
        s.node);
  }

  void execBlockInherited(VM& vm, const AST::BlockStmt& b) {
    ScopeGuard guard(vm);
    for (auto const& sp : b.statements) {
      vm.exec(*sp);
    }
  }

  void run(const AST::Program& p) {
    try {
      for (auto const& st : p.statements) {
        exec(*st);
      }
      result.ok = true;
    } catch (const std::exception& e) {
      result.ok = false;
      result.error = e.what();
    }
  }
};

}  // namespace

ExecResult interpret(const AST::Program& program) {
  VM vm;
  vm.run(program);
  return vm.result;
}

std::string toStringRuntime(const LiteralValue& v) {
  return valueToString(v);
}
