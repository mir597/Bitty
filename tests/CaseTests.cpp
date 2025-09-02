#include <gtest/gtest.h>

#include <filesystem>
#include <sstream>

#include "BittyLexer.h"
#include "BittyParser.h"
#include "antlr4-runtime.h"
#include "common/AST.hpp"
#include "common/Bytecode.hpp"
#include "compiler/ConstantPropagation.hpp"
#include "compiler/IL2Bytecode.hpp"
#include "compiler/ILOptimize.hpp"
#include "compiler/Lowering.hpp"
#include "compiler/TypeInference.hpp"
#include "frontend/ASTBuilder.hpp"
#include "utils/ASTEqual.hpp"
#include "utils/ILVerify.hpp"
#include "utils/TestUtils.hpp"
#include "vm/BCInterpreter.hpp"

#ifndef BITTY_SOURCE_DIR
#define BITTY_SOURCE_DIR "."
#endif

using namespace bitty;

static std::string CaseNameGen(
    const testing::TestParamInfo<std::string>& info) {
  return std::filesystem::path(info.param).stem().string();
}

static std::vector<std::string> allCases() {
  std::filesystem::path p =
      std::filesystem::path(BITTY_SOURCE_DIR) / "tests" / "inputs";
  return collect_case_files(p.string());
}

class CaseParamTest : public ::testing::TestWithParam<std::string> {};

TEST_P(CaseParamTest, ASTRoundTripFromFile) {
  const std::string path = GetParam();
  if (path.find("noast") != std::string::npos) {
    GTEST_SKIP() << "Skip test";
  }
  SCOPED_TRACE("case: " + path);
  const std::string src = readFileText(path);

  AST::Program ast0 = parseToAST(src);

  std::string pretty = AST::toSource(ast0);
  AST::Program ast1 = parseToAST(pretty);

  ASSERT_TRUE(eqProgram(ast0, ast1))
      << "AST mismatch after pretty-print roundtrip for: " << path;
}

TEST_P(CaseParamTest, ILLowering_VerifyAndCallCounts) {
  const std::string path = GetParam();
  SCOPED_TRACE("case: " + path);
  const std::string src = readFileText(path);

  AST::Program ast = parseToAST(src);
  auto calls = countAstCalls(ast);
  auto exprCalls = countAstExprStmtCalls(ast);

  IL::Program il = IL::lowerFromAST(ast);
  if (std::getenv("BITTY_DUMP_IL")) {
    std::cerr << "\n=== IL of " << path << " ===\n" << IL::toSource(il) << "\n";
  }
  auto vr = IL::verify(il, /*strictSymbols=*/false);
  ASSERT_TRUE(vr.ok) << (vr.errors.empty() ? "" : vr.errors.front());

  EXPECT_EQ(calls, countIlCallStmts(il));
  EXPECT_EQ(exprCalls, countIlVoidCalls(il));
}

TEST_P(CaseParamTest, ILLowering_RoundTripIdempotentOnIL) {
  const std::string path = GetParam();
  SCOPED_TRACE("case: " + path);
  if (path.find("nonsym") != std::string::npos) {
    GTEST_SKIP() << "Skip test";
  }
  const std::string src = readFileText(path);

  AST::Program ast0 = parseToAST(src);
  IL::Program il1 = IL::lowerFromAST(ast0);
  auto v1 = IL::verify(il1, false);
  ASSERT_TRUE(v1.ok) << (v1.errors.empty() ? "" : v1.errors.front());

  std::string ilSrc1 = IL::toSource(il1);
  AST::Program ast1 = parseToAST(ilSrc1);
  IL::Program il2 = IL::lowerFromAST(ast1);
  auto v2 = IL::verify(il2, false);
  ASSERT_TRUE(v2.ok) << (v2.errors.empty() ? "" : v2.errors.front());

  // normalize to ignore spaces
  std::string n1 = normalizeILText(ilSrc1);
  std::string n2 = normalizeILText(IL::toSource(il2));
  EXPECT_EQ(n1, n2);
}

void snapshot(const std::string& path, const std::string& outfolder,
              std::function<std::string(const AST::Program& ast)> process) {
  namespace fs = std::filesystem;
  const std::string src = readFileText(path);

  AST::Program ast = parseToAST(src);
  std::string ilText = process(ast);

  fs::path outDir =
      fs::path(BITTY_SOURCE_DIR) / "tests" / "outputs" / outfolder;
  fs::create_directories(outDir);
  fs::path snapPath = outDir / (fs::path(path).stem().string() + ".il");

  if (shouldUpdateSnapshots()) {
    writeFileText(snapPath.string(), ilText);
    SUCCEED() << "snapshot updated: " << snapPath;
    return;
  }

  if (!fs::exists(snapPath)) {
    ADD_FAILURE() << "snapshot not found: " << snapPath
                  << "\nRun with BITTY_UPDATE_SNAPSHOTS=1 to create it.";
    return;
  }

  std::string expected = readFileText(snapPath.string());
  EXPECT_EQ(ilText, expected)
      << "IL snapshot mismatch for: " << path << "\nSnapshot: " << snapPath;
}

// ========== IL snapshot ==========
TEST_P(CaseParamTest, ILSnapshot) {
  const std::string path = GetParam();
  snapshot(path, "il", [&](const AST::Program& ast) -> std::string {
    IL::Program il = IL::lowerFromAST(ast);
    auto vr = IL::verify(il, /*strictSymbols=*/false);
    EXPECT_TRUE(vr.ok) << (vr.errors.empty() ? "" : vr.errors.front());
    return IL::toSource(il);
  });
}

TEST_P(CaseParamTest, ILConstantPropagationSnapshot) {
  const std::string path = GetParam();
  snapshot(path, "cp", [&](const AST::Program& ast) -> std::string {
    IL::Program il = IL::lowerFromAST(ast);
    IL::propagateConstants(il);
    auto vr = IL::verify(il, /*strictSymbols=*/false);
    EXPECT_TRUE(vr.ok) << (vr.errors.empty() ? "" : vr.errors.front());
    return IL::toSource(il);
  });
}

TEST_P(CaseParamTest, ILTypeInferenceSnapshot) {
  const std::string path = GetParam();
  snapshot(path, "typedil", [&](const AST::Program& ast) -> std::string {
    IL::Program il = IL::lowerFromAST(ast);
    IL::inferTypes(il);
    auto vr = IL::verify(il, /*strictSymbols=*/false);
    EXPECT_TRUE(vr.ok) << (vr.errors.empty() ? "" : vr.errors.front());
    return IL::toSource(il);
  });
}

TEST_P(CaseParamTest, ILOptimizationSnapshot) {
  const std::string path = GetParam();
  snapshot(path, "opt", [&](const AST::Program& ast) -> std::string {
    IL::Program il = IL::lowerFromAST(ast);
    IL::optimizeProgram(il);
    auto vr = IL::verify(il, /*strictSymbols=*/false);
    EXPECT_TRUE(vr.ok) << (vr.errors.empty() ? "" : vr.errors.front());
    return IL::toSource(il);
  });
}

TEST_P(CaseParamTest, ILConstProp_SemanticsPreserv) {
  const std::string path = GetParam();
  SCOPED_TRACE("case (const-prop): " + path);
  const std::string src = readFileText(path);

  AST::Program ast = parseToAST(src);
  IL::Program il = IL::lowerFromAST(ast);

  BC::Module mod = BC::compileFromIL(il);
  auto r1 = BC::runBytecode(mod);

  IL::propagateConstants(il);

  BC::Module mod2 = BC::compileFromIL(il);
  auto r2 = BC::runBytecode(mod);

  EXPECT_EQ(r1.ok, r2.ok);
  EXPECT_EQ(r1.stdout_text, r2.stdout_text);
}

TEST_P(CaseParamTest, ILConstProp_RoundTripIdempotentOnIL) {
  const std::string path = GetParam();
  SCOPED_TRACE("case (const-prop): " + path);
  const std::string src = readFileText(path);

  AST::Program ast0 = parseToAST(src);
  IL::Program il1 = IL::lowerFromAST(ast0);

  IL::propagateConstants(il1);

  auto v = IL::verify(il1, false);
  ASSERT_TRUE(v.ok) << (v.errors.empty() ? "" : v.errors.front());

  std::string ilSrc1 = IL::toSource(il1);

  IL::propagateConstants(il1);

  auto v2 = IL::verify(il1, false);
  ASSERT_TRUE(v2.ok) << (v2.errors.empty() ? "" : v2.errors.front());

  std::string ilSrc2 = IL::toSource(il1);

  EXPECT_EQ(normalizeILText(ilSrc1), normalizeILText(ilSrc2));
}

TEST_P(CaseParamTest, ILOptimize_SemanticsPreserv) {
  const std::string path = GetParam();
  SCOPED_TRACE("case (optimize): " + path);
  const std::string src = readFileText(path);

  AST::Program ast = parseToAST(src);
  IL::Program il = IL::lowerFromAST(ast);

  BC::Module mod = BC::compileFromIL(il);
  auto r1 = BC::runBytecode(mod);

  IL::optimizeProgram(il);

  BC::Module mod2 = BC::compileFromIL(il);
  auto r2 = BC::runBytecode(mod);

  EXPECT_EQ(r1.ok, r2.ok);
  EXPECT_EQ(r1.stdout_text, r2.stdout_text);
}

INSTANTIATE_TEST_SUITE_P(AllCases, CaseParamTest,
                         ::testing::ValuesIn(allCases()), CaseNameGen);
