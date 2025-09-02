
#include <gtest/gtest.h>

#include <optional>
#include <regex>
#include <string>
#include <unordered_map>

#include "common/AST.hpp"
#include "common/Bytecode.hpp"
#include "common/IL.hpp"
#include "compiler/IL2Bytecode.hpp"
#include "compiler/ILOptimize.hpp"
#include "frontend/ASTBuilder.hpp"
#include "utils/TestUtils.hpp"
#include "vm/BCInterpreter.hpp"

using namespace bitty;
using namespace bitty::IL;
using namespace bitty::BC;

// -------------------- tests --------------------

// 1) CSE basic: (a+b)*(a+b) -> ADD once, MUL once
TEST(IL_Optimize_Bitty, CSE_SimpleSquares) {
  const char* src = R"(
    let a = 10;
    let b = 20;
    print (a + b) * (a + b);
  )";
  auto P = runFromSource(src);
  EXPECT_EQ(P.outText, "900\n");
  EXPECT_EQ(countOp(P.mod, Op::ADD), 1);
  EXPECT_EQ(countOp(P.mod, Op::MUL), 1);

  auto norm = normalizeGensyms(P.ilAfterSrc);
  EXPECT_NE(norm.find("let _cse$"), std::string::npos);
  EXPECT_NE(norm.find("_cse$ = a + b;"), std::string::npos);
}

// 2) CSE across statements but blocked by version bump
TEST(IL_Optimize_Bitty, CSE_VersionedAcrossStatements) {
  const char* src = R"(
    let a = 5;
    print a + a;
    print a + a;
    a = 6;
    print a + a;
  )";
  auto P = runFromSource(src);
  EXPECT_EQ(P.outText, "10\n10\n12\n");
  EXPECT_EQ(countOp(P.mod, Op::ADD), 2);
}

// 3) CSE inside if-then block only
TEST(IL_Optimize_Bitty, CSE_IfThenBlock) {
  const char* src = R"(
    let a = 3;
    if (a + 0 == 3) { print (a * 2) + (a * 2); }
    else { print 0; }
  )";
  auto P = runFromSource(src);
  EXPECT_EQ(P.outText, "12\n");
  EXPECT_EQ(countOp(P.mod, Op::MUL), 1);
}

// 4) CSE inside while-body per iteration
TEST(IL_Optimize_Bitty, CSE_WhileBody) {
  const char* src = R"(
    let i = 0;
    while (i < 2) {
      print (i * 2) + (i * 2);
      i = i + 1;
    }
  )";
  auto P = runFromSource(src);
  EXPECT_EQ(P.outText, "0\n4\n");
  EXPECT_EQ(countOp(P.mod, Op::MUL), 1);
}

// 5) CSE on unary (−x)
TEST(IL_Optimize_Bitty, CSE_UnaryNegate) {
  const char* src = R"(
    let x = 7;
    print (-x) + (-x);
  )";
  auto P = runFromSource(src);
  EXPECT_EQ(P.outText, "-14\n");
  EXPECT_EQ(countOp(P.mod, Op::ADD), 1);
}

// 6) CSE with string semantics: (s*3)+(s*3)
TEST(IL_Optimize_Bitty, CSE_StringsRepeatConcat) {
  const char* src = R"(
    let s = "ha";
    print (s * 3) + (s * 3);
  )";
  auto P = runFromSource(src);
  EXPECT_EQ(P.outText, "hahahahahaha\n");
  EXPECT_EQ(countOp(P.mod, Op::MUL), 1);
  EXPECT_EQ(countOp(P.mod, Op::ADD), 1);
}

// 7) No commutativity assumed: (a+b)+(b+a) not CSE’d
TEST(IL_Optimize_Bitty, CSE_CommutativityNotAssumed) {
  const char* src = R"(
    let a = 2;
    let b = 5;
    print (a + b) + (b + a);
  )";
  auto P = runFromSource(src);
  EXPECT_EQ(P.outText, "14\n");
  EXPECT_GE(countOp(P.mod, Op::ADD), 2);
}

// 8) Inline: single-use simple function (value return)
TEST(IL_Optimize_Bitty, Inline_SingleUse_Value) {
  const char* src = R"(
    fn add1(x) { return x + 1; }
    let a = 41;
    let r = add1(a);
    print r;
  )";
  auto P = runFromSource(src);
  EXPECT_EQ(P.outText, "42\n");
  EXPECT_EQ(countOp(P.mod, Op::CALL), 0);
  EXPECT_FALSE(hasFunc(P.mod, "add1"));
  EXPECT_EQ(P.ilAfterSrc.find("fn add1("), std::string::npos);
}

// 9) Inline: single-use void; target gets nil
TEST(IL_Optimize_Bitty, Inline_SingleUse_VoidTargetGetsNil) {
  const char* src = R"(
    fn log2(x) { print x; return; }
    let t = 7;
    let sink = log2(t);
  )";
  auto P = runFromSource(src);
  EXPECT_EQ(P.outText, "7\n");
  EXPECT_EQ(countOp(P.mod, Op::CALL), 0);
  EXPECT_FALSE(hasFunc(P.mod, "log2"));
  auto norm = normalizeGensyms(P.ilAfterSrc);
  EXPECT_NE(norm.find("sink = nil;"), std::string::npos);
}

// 10) Inline: missing argument defaults to nil
TEST(IL_Optimize_Bitty, Inline_MissingArgDefaultsToNil) {
  const char* src = R"(
    fn is_nil(x) { return x == 0; }
    let r = is_nil();
    print r;
  )";
  auto P = runFromSource(src);
  EXPECT_EQ(P.outText, "true\n");
  EXPECT_EQ(countOp(P.mod, Op::CALL), 0);
  EXPECT_FALSE(hasFunc(P.mod, "is_nil"));
  auto norm = normalizeGensyms(P.ilAfterSrc);
  EXPECT_NE(norm.find("_inl_p$ = nil;"), std::string::npos);
}

// 11) Inline not applied: multiple returns
TEST(IL_Optimize_Bitty, Inline_NotApplied_MultiReturn) {
  const char* src = R"(
    fn sign(x) { if (x < 0) return -1; return 1; }
    let r = sign(5);
    print r;
  )";
  auto P = runFromSource(src);
  EXPECT_EQ(P.outText, "1\n");
  EXPECT_GE(countOp(P.mod, Op::CALL), 1);
  EXPECT_TRUE(hasFunc(P.mod, "sign"));
  EXPECT_NE(P.ilAfterSrc.find("fn sign("), std::string::npos);
}

// 12) Inline not applied: return not at tail
TEST(IL_Optimize_Bitty, Inline_NotApplied_TrailingStmtAfterReturn) {
  const char* src = R"(
    fn id(x) { return x; print 0; }
    let r = id(5);
    print r;
  )";
  auto P = runFromSource(src);
  EXPECT_EQ(P.outText, "0\n5\n");
  EXPECT_GE(countOp(P.mod, Op::CALL), 1);
  EXPECT_TRUE(hasFunc(P.mod, "id"));
}

// 13) CSE across two prints
TEST(IL_Optimize_Bitty, CSE_AcrossTwoPrints) {
  const char* src = R"(
    let a = 2;
    let b = 5;
    print a + b;
    print a + b;
  )";
  auto P = runFromSource(src);
  EXPECT_EQ(P.outText, "7\n7\n");
  EXPECT_EQ(countOp(P.mod, Op::ADD), 1);
}
