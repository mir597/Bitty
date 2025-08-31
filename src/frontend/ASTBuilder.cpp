#include "frontend/ASTBuilder.hpp"

#include <cstdlib>
#include <stdexcept>
#include <utility>

#include "common/Common.hpp"
#include "common/LiteralValue.hpp"

using Parser = bitty::parser::BittyParser;
using antlr4::tree::TerminalNode;
using namespace bitty;

namespace {
std::string unescapeStringBody(std::string_view inner) {
  std::string out;
  out.reserve(inner.size());
  for (size_t i = 0; i < inner.size(); ++i) {
    char c = inner[i];
    if (c == '\\' && i + 1 < inner.size()) {
      char n = inner[i + 1];
      switch (n) {
        case '\\':
          out.push_back('\\');
          break;
        case '"':
          out.push_back('"');
          break;
        case 'n':
          out.push_back('\n');
          break;
        case 't':
          out.push_back('\t');
          break;
        case 'r':
          out.push_back('\r');
          break;
        default:
          out.push_back('\\');
          out.push_back(n);
          break;
      }
      ++i;
    } else {
      out.push_back(c);
    }
  }
  return out;
}
}  // namespace

AST::Program ASTBuilder::build(Parser::ProgramContext* program) {
  AST::Program out;
  for (auto* s : program->statement()) {
    auto st = buildStatement(s);
    if (st) {
      out.statements.emplace_back(std::move(st));
    }
  }
  return out;
}

// ---------------- statements ----------------
std::unique_ptr<AST::Statement> ASTBuilder::buildStatement(
    Parser::StatementContext* s) {
  if (!s) {
    throw std::runtime_error("buildStmt: null ctx");
  }

  if (auto* b = dynamic_cast<Parser::StmtBlockContext*>(s)) {
    return buildBlock(b->blockStmt());
  }
  if (auto* v = dynamic_cast<Parser::StmtVarDeclContext*>(s)) {
    return buildVarDecl(v->varDeclStmt());
  }
  if (auto* i = dynamic_cast<Parser::StmtIfContext*>(s)) {
    return buildIf(i->ifStmt());
  }
  if (auto* w = dynamic_cast<Parser::StmtWhileContext*>(s)) {
    return buildWhile(w->whileStmt());
  }
  if (auto* r = dynamic_cast<Parser::StmtReturnContext*>(s)) {
    return buildReturn(r->returnStmt());
  }
  if (auto* f = dynamic_cast<Parser::StmtFuncDeclContext*>(s)) {
    return buildFuncDecl(f->funcDeclStmt());
  }
  if (auto* e = dynamic_cast<Parser::StmtExpressionContext*>(s)) {
    return buildExprStmt(e->expression());
  }
  if (auto* p = dynamic_cast<Parser::StmtPrintContext*>(s)) {
    return buildPrintStmt(p->printStmt()->expression());
  }
  throw std::runtime_error("unknown StatementContext alternative");
}

std::unique_ptr<AST::Statement> ASTBuilder::buildBlock(
    Parser::BlockStmtContext* b) {
  AST::BlockStmt blk;
  for (auto* s : b->statement()) {
    auto st = buildStatement(s);
    if (st) {
      blk.statements.emplace_back(std::move(st));
    }
  }
  return create<AST::Statement>(std::move(blk));
}

std::unique_ptr<AST::Statement> ASTBuilder::buildVarDecl(
    Parser::VarDeclStmtContext* s) {
  AST::VarDeclStmt vd;
  vd.name = s->ID()->getText();
  if (s->ASSIGN()) {
    vd.initializer = buildExpression(s->expression());
  }
  return create<AST::Statement>(std::move(vd));
}

std::unique_ptr<AST::Statement> ASTBuilder::buildIf(Parser::IfStmtContext* s) {
  AST::IfStmt is;
  is.condition = buildExpression(s->expression());

  // then
  is.thenBranch = buildStatement(s->statement(0));

  // else
  if (s->statement().size() > 1) {
    is.elseBranch = buildStatement(s->statement(1));
  } else {
    is.elseBranch.reset();
  }

  return create<AST::Statement>(std::move(is));
}

std::unique_ptr<AST::Statement> ASTBuilder::buildWhile(
    Parser::WhileStmtContext* s) {
  AST::WhileStmt ws;
  ws.condition = buildExpression(s->expression());
  ws.body = buildStatement(s->statement());
  return create<AST::Statement>(std::move(ws));
}

std::unique_ptr<AST::Statement> ASTBuilder::buildReturn(
    Parser::ReturnStmtContext* s) {
  AST::ReturnStmt rs;
  if (s->expression()) {
    rs.value = buildExpression(s->expression());
  }
  return create<AST::Statement>(std::move(rs));
}

std::unique_ptr<AST::Statement> ASTBuilder::buildExprStmt(
    Parser::ExpressionContext* e) {
  AST::ExpressionStmt es;
  es.expression = buildExpression(e);
  return create<AST::Statement>(es);
}

std::unique_ptr<AST::Statement> ASTBuilder::buildPrintStmt(
    Parser::ExpressionContext* e) {
  auto es = buildExpression(e);
  return create<AST::Statement>(AST::PrintStmt{std::move(es)});
}

std::unique_ptr<AST::Statement> ASTBuilder::buildFuncDecl(
    Parser::FuncDeclStmtContext* ctx) {
  AST::FuncDeclStmt f{};
  f.name = ctx->ID()->getText();
  if (auto* plist = ctx->paramList()) {
    for (auto* idTok : plist->ID()) {
      f.params.push_back(idTok->getText());
    }
  }
  auto block = std::make_unique<AST::BlockStmt>();
  for (auto* stx : ctx->blockStmt()->statement()) {
    block->statements.push_back(buildStatement(stx));
  }
  f.body = std::move(block);

  return create<AST::Statement>(std::move(f));
}
// ---------------- expressions ----------------
std::unique_ptr<AST::Expression> ASTBuilder::buildExpression(
    Parser::ExpressionContext* e) {
  if (auto* p = dynamic_cast<Parser::ExprPrimaryContext*>(e)) {
    return buildPrimary(p->primary());
  } else if (auto* c = dynamic_cast<Parser::ExprCallContext*>(e)) {
    return buildCall(c);
  } else if (auto* u = dynamic_cast<Parser::ExprUnaryContext*>(e)) {
    return buildUnary(u);
  } else if (auto* md = dynamic_cast<Parser::ExprLogicalAndContext*>(e)) {
    return buildBinary(md->left, md->op, md->right);
  } else if (auto* md = dynamic_cast<Parser::ExprLogicalOrContext*>(e)) {
    return buildBinary(md->left, md->op, md->right);
  } else if (auto* md = dynamic_cast<Parser::ExprMulDivContext*>(e)) {
    return buildBinary(md->left, md->op, md->right);
  } else if (auto* as = dynamic_cast<Parser::ExprAddSubContext*>(e)) {
    return buildBinary(as->left, as->op, as->right);
  } else if (auto* cm = dynamic_cast<Parser::ExprCompareContext*>(e)) {
    return buildBinary(cm->left, cm->op, cm->right);
  } else if (auto* eq = dynamic_cast<Parser::ExprEqualityContext*>(e)) {
    return buildBinary(eq->left, eq->op, eq->right);
  } else if (auto* asg = dynamic_cast<Parser::ExprAssignContext*>(e)) {
    return buildAssign(asg);
  }
  throw std::runtime_error("unknown ExpressionContext alternative");
}

std::unique_ptr<AST::Expression> ASTBuilder::buildPrimary(
    Parser::PrimaryContext* p) {
  if (p->LPAREN()) {
    return buildExpression(p->expression());
  }
  if (p->literal()) {
    AST::LiteralExpr le;
    le.value = buildLiteral(p->literal());
    return create<AST::Expression>(std::move(le));
  }
  if (p->ID()) {
    AST::VariableExpr vx;
    vx.name = p->ID()->getText();
    return create<AST::Expression>(std::move(vx));
  }
  throw std::runtime_error("primary: unexpected alternative");
}

std::unique_ptr<AST::Expression> ASTBuilder::buildCall(
    Parser::ExprCallContext* e) {
  AST::CallExpr ce;
  ce.callee = buildExpression(e->expression());

  if (e->exprList()) {
    auto* list = e->exprList();
    ce.arguments.reserve(list->expression().size());
    for (auto* a : list->expression()) {
      ce.arguments.emplace_back(buildExpression(a));
    }
  }

  return create<AST::Expression>(std::move(ce));
}

std::unique_ptr<AST::Expression> ASTBuilder::buildUnary(
    Parser::ExprUnaryContext* e) {
  AST::UnaryExpr ue;
  ue.op = toUnaryOp(e->op->getText());
  ue.right = buildExpression(e->expression());

  return create<AST::Expression>(std::move(ue));
}

std::unique_ptr<AST::Expression> ASTBuilder::buildBinary(
    Parser::ExpressionContext* L, antlr4::Token* opTok,
    Parser::ExpressionContext* R) {
  AST::BinaryExpr be;
  be.left = buildExpression(L);
  be.op = toBinaryOp(opTok->getText());
  be.right = buildExpression(R);

  return create<AST::Expression>(std::move(be));
}

std::unique_ptr<AST::Expression> ASTBuilder::buildAssign(
    Parser::ExprAssignContext* e) {
  AST::AssignmentExpr asn;

  auto t = std::make_unique<AST::Expression>();
  t->node = AST::VariableExpr{e->lhs->getText()};
  asn.target = std::move(t);

  asn.value = buildExpression(e->rhs);

  return create<AST::Expression>(std::move(asn));
}

LiteralValue ASTBuilder::buildLiteral(
    bitty::parser::BittyParser::LiteralContext* lit) {
  if (!lit) {
    throw std::runtime_error("buildLiteral: null context");
  }

  // number
  if (auto* tok = lit->NUMBER()) {
    const std::string s = tok->getText();
    if (s.find('.') != std::string::npos) {
      return LiteralValue{std::stod(s)};
    } else {
      try {
        long long v = std::stoll(s);
        return LiteralValue{static_cast<int64_t>(v)};
      } catch (...) {
        return LiteralValue{std::stod(s)};
      }
    }
  }

  // string
  if (auto* tok = lit->STRING()) {
    const std::string raw = tok->getText();
    std::string_view body(raw.data() + 1, raw.size() - 2);
    auto val = unescapeStringBody(body);
    return LiteralValue{std::move(val)};
  }

  // booleans / nil
  if (lit->TRUE()) {
    return LiteralValue{true};
  } else if (lit->FALSE()) {
    return LiteralValue{false};
  } else if (lit->NIL()) {
    return LiteralValue{std::monostate{}};
  }

  throw std::runtime_error("buildLiteral: unknown literal kind");
}
