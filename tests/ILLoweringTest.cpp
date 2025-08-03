#include <gtest/gtest.h>

#include <sstream>

#include "BittyLexer.h"
#include "BittyParser.h"
#include "antlr4-runtime.h"
#include "core/AST.hpp"
#include "core/ASTBuilder.hpp"
#include "core/Lowering.hpp"
#include "utils/ASTEqual.hpp"
#include "utils/ILVerify.hpp"
#include "utils/TestUtils.hpp"

using namespace bitty;

TEST(ILLowering, StringsAndEscapes) {
  const char* src = R"(
    fn p() {
      print "hello\nworld";
      print "quote:\" backslash:\\";
      return "ok";
    }
    p();
  )";
  auto ast = parseToAST(src);
  auto il = IL::lowerFromAST(ast);
  auto vr = IL::verify(il, false);
  ASSERT_TRUE(vr.ok) << (vr.errors.empty() ? "" : vr.errors.front());

  std::string txt = IL::toSource(il);
  auto ast2 = parseToAST(txt);
  auto il2 = IL::lowerFromAST(ast2);
  auto vr2 = IL::verify(il2, false);
  ASSERT_TRUE(vr2.ok);
  EXPECT_EQ(normalizeILText(txt), normalizeILText(IL::toSource(il2)));
}
