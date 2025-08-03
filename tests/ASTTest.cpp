#include <gtest/gtest.h>

#include "utils/ASTEqual.hpp"
#include "utils/TestUtils.hpp"

using namespace bitty;

namespace {
static void ExpectRoundTripEq(const std::string& src) {
  AST::Program ast1 = parseToAST(src);
  std::string pretty = AST::toSource(ast1);
  AST::Program ast2 = parseToAST(pretty);
  std::string pretty2 = AST::toSource(ast2);
  EXPECT_TRUE(eqProgram(ast1, ast2)) << "Pretty-printed:\n" << pretty;
}
}  // namespace

TEST(LanguageShape, AssignmentIsRightAssociative) {
  auto prog = parseToAST("a = b = c;");
  ASSERT_EQ(prog.statements.size(), 1u);

  const AST::Statement& s = *prog.statements[0];
  auto* es = std::get_if<AST::ExpressionStmt>(&s.node);
  ASSERT_NE(es, nullptr);

  auto* asn1 = std::get_if<AST::AssignmentExpr>(&es->expression->node);
  ASSERT_NE(asn1, nullptr);

  // outer target == "a"
  auto* lhs1 = std::get_if<AST::VariableExpr>(&asn1->target->node);
  ASSERT_NE(lhs1, nullptr);
  EXPECT_EQ(lhs1->name, "a");

  // outer value is another assignment (b = c)
  auto* asn2 = std::get_if<AST::AssignmentExpr>(&asn1->value->node);
  ASSERT_NE(asn2, nullptr);

  auto* lhs2 = std::get_if<AST::VariableExpr>(&asn2->target->node);
  ASSERT_NE(lhs2, nullptr);
  EXPECT_EQ(lhs2->name, "b");

  auto* rhs2 = std::get_if<AST::VariableExpr>(&asn2->value->node);
  ASSERT_NE(rhs2, nullptr);
  EXPECT_EQ(rhs2->name, "c");
}

TEST(ParserNegative, AssignmentLHS_MustBeID_Literal) {
  EXPECT_THROW(parseToAST("1 = 2;"), std::runtime_error);
}
TEST(ParserNegative, AssignmentLHS_MustBeID_Binary) {
  EXPECT_THROW(parseToAST("(a + b) = 3;"), std::runtime_error);
}

TEST(LanguageShape, UnaryVsCallPrecedence) {
  auto prog = parseToAST("-f(1);");
  ASSERT_EQ(prog.statements.size(), 1u);
  const AST::Statement& s = *prog.statements[0];
  auto* es = std::get_if<AST::ExpressionStmt>(&s.node);
  ASSERT_NE(es, nullptr);

  auto* un = std::get_if<AST::UnaryExpr>(&es->expression->node);
  ASSERT_NE(un, nullptr);
  EXPECT_EQ(un->op, UnaryOp::UNM);

  auto* call = std::get_if<AST::CallExpr>(&un->right->node);
  ASSERT_NE(call, nullptr);

  auto* callee = std::get_if<AST::VariableExpr>(&call->callee->node);
  ASSERT_NE(callee, nullptr);
  EXPECT_EQ(callee->name, "f");
  ASSERT_EQ(call->arguments.size(), 1u);
}
