#include <gtest/gtest.h>

#include <string>

#include "BittyLexer.h"
#include "BittyParser.h"
#include "antlr4-runtime.h"
#include "common/AST.hpp"
#include "frontend/ASTBuilder.hpp"
#include "interpreter/Interpreter.hpp"
#include "utils/TestUtils.hpp"

using namespace bitty;

TEST(Interpreter, ArithmeticAndVars) {
  const char* src = R"(
    let x = 1 + 2 * 3;
    let y;
    y = (1 + 2) * 3;
    print x;
    print y;
  )";
  auto ast = parseToAST(src);
  auto r = interpret(ast);
  ASSERT_TRUE(r.ok) << r.error;
  EXPECT_EQ(r.stdout_text, "7\n9\n");
}

TEST(Interpreter, StringsAndConcatRepeat) {
  const char* src = R"(
    print "a" + "b";
    print "hi" * 3;
    print "x" + 1;
    print 1 + "x";
    print 1 + 2 + "x";
  )";
  auto ast = parseToAST(src);
  auto r = interpret(ast);
  ASSERT_TRUE(r.ok) << r.error;
  EXPECT_EQ(r.stdout_text, "ab\nhihihi\nx1\n1x\n3x\n");
}

TEST(Interpreter, ComparisonsAndEq) {
  const char* src = R"(
    print 3 < 5;
    print "a" < "b";
    print "10" < 2;
    print "10" == 10;
    print 0 == false;
    print "" == false;
    print nil == 0;
  )";
  auto ast = parseToAST(src);
  auto r = interpret(ast);
  ASSERT_TRUE(r.ok) << r.error;
  EXPECT_EQ(r.stdout_text, "true\ntrue\nfalse\ntrue\ntrue\ntrue\ntrue\n");
}

TEST(Interpreter, IfElseWhile) {
  const char* src = R"(
    let x = 0;
    if (x) print "T"; else print "F";
    x = 2;
    if (x) print "T"; else print "F";
    let i = 0;
    let s = "";
    while (i < 3) {
      s = s + i;
      i = i + 1;
    }
    print s;
  )";
  auto ast = parseToAST(src);
  auto r = interpret(ast);
  ASSERT_TRUE(r.ok) << r.error;
  EXPECT_EQ(r.stdout_text, "F\nT\n012\n");
}

TEST(Interpreter, FunctionsAndCalls) {
  const char* src = R"(
    fn add(a, b) { return a + b; }
    print add(2, 3);

    fn inc(a) { print a; return a + 1; }
    let x = inc(41);
    print x;
  )";
  auto ast = parseToAST(src);
  auto r = interpret(ast);
  ASSERT_TRUE(r.ok) << r.error;
  EXPECT_EQ(r.stdout_text, "5\n41\n42\n");
}

TEST(Interpreter, WhileWithFunctionInCondition) {
  const char* src = R"(
    fn f(x) { return x + 1; }
    let x = 0;
    while (f(x) < 3) {
      x = x + 1;
    }
    print x;
  )";
  auto ast = parseToAST(src);
  auto r = interpret(ast);
  ASSERT_TRUE(r.ok) << r.error;
  EXPECT_EQ(r.stdout_text, "2\n");
}

TEST(Interpreter, StringEscapesPrinting) {
  const char* src = R"(
    print "a\nb";
    print "quote:\"";
    print "backslash:\\";
    print "tab:\t-";
  )";
  auto ast = parseToAST(src);
  auto r = interpret(ast);
  ASSERT_TRUE(r.ok) << r.error;
  EXPECT_EQ(r.stdout_text, std::string("a\nb\n") + "quote:\"\n" +
                               "backslash:\\\n" + "tab:\t-\n");
}
