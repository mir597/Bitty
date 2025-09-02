#include <gtest/gtest.h>

#include <string>

#include "common/AST.hpp"
#include "common/Bytecode.hpp"
#include "common/IL.hpp"
#include "compiler/AST2IL.hpp"
#include "compiler/ConstantPropagation.hpp"
#include "compiler/IL2Bytecode.hpp"
#include "utils/ILVerify.hpp"
#include "utils/TestUtils.hpp"
#include "vm/BCInterpreter.hpp"

using namespace bitty;

static std::string runBC(IL::Program& il) {
  BC::Module mod = BC::compileFromIL(il);
  auto r = BC::runBytecode(mod);
  if (!r.ok) {
    ADD_FAILURE() << "runtime error: " << r.error;
  }
  return r.stdout_text;
}

static void expectSameOutput(const std::string& src) {
  AST::Program ast = parseToAST(src);
  IL::Program il = IL::lowerFromAST(ast);
  auto v1 = IL::verify(il, /*strictSymbols=*/false);
  EXPECT_TRUE(v1.ok) << (v1.errors.empty() ? "" : v1.errors.front());

  const std::string out0 = runBC(il);

  IL::propagateConstants(il);

  auto v2 = IL::verify(il, /*strictSymbols=*/false);
  const std::string out1 = runBC(il);
  EXPECT_EQ(out0, out1);
}

static void expectIdempotentIL(const std::string& src) {
  AST::Program ast = parseToAST(src);
  IL::Program il = IL::lowerFromAST(ast);

  IL::propagateConstants(il);
  std::string once = normalizeILText(IL::toSource(il));

  IL::propagateConstants(il);
  std::string twice = normalizeILText(IL::toSource(il));

  EXPECT_EQ(once, twice);
}

TEST(ConstantPropagation, FoldsPureArithmeticAndStrings) {
  const char* src = R"(
    print 1 + 2 * 3;
    print "ha" * 3;
    print "a" + 1;
    print 2 + "b";
    print "x" + "y";
    print (10 - 3) * 2;
  )";

  expectSameOutput(src);
  expectIdempotentIL(src);
}

TEST(ConstantPropagation, PropagatesAcrossAssignments) {
  const char* src = R"(
    let x; x = 5;
    let y; y = x + 2;
    print y;
  )";
  expectSameOutput(src);
  expectIdempotentIL(src);
}

TEST(ConstantPropagation, IfBranchSelectionAndJoin) {
  {
    const char* src = R"(
      let x;
      if (true) { x = 1; } else { x = 2; }
      print x;
    )";
    expectSameOutput(src);
    expectIdempotentIL(src);
  }
  {
    const char* src = R"(
      fn f(p) {
        let x;
        if (p) { x = 1; } else { x = 2; }
        print x;
      }
      f(true);
      f(false);
    )";
    expectSameOutput(src);
    expectIdempotentIL(src);
  }
}

TEST(ConstantPropagation, WhileCounterBoundIsSafe) {
  const char* src = R"(
    let i; i = 0;
    while (i < 3) { i = i + 1; }
    print i;
  )";
  expectSameOutput(src);
  expectIdempotentIL(src);
}

TEST(ConstantPropagation, WhileWithHoistedCallConditionIsSafe) {
  const char* src = R"(
    fn less3(x) { return x < 3; }
    let i; i = 0;
    while (less3(i)) { i = i + 1; }
    print i;
  )";
  expectSameOutput(src);
  expectIdempotentIL(src);
}

TEST(ConstantPropagation, CallReturnConstant) {
  const char* src = R"(
    fn seven() { return 7; }
    print seven();
  )";
  expectSameOutput(src);
  expectIdempotentIL(src);
}

TEST(ConstantPropagation, CallContextInsensitiveParamJoinToTop) {
  const char* src = R"(
    fn id(a) { return a; }
    print id(3);
    print id(4);
  )";
  expectSameOutput(src);
  expectIdempotentIL(src);
}

TEST(ConstantPropagation, TruthinessAndEqualityMatchRuntime) {
  const char* src = R"(
    print (0 == false);
    print ("1" == 1);
    print (!"");
    print (!!0);
  )";
  expectSameOutput(src);
  expectIdempotentIL(src);
}
