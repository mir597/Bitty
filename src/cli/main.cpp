#include <fstream>
#include <iostream>
#include <memory>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

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
#include "interpreter/Interpreter.hpp"
#include "vm/BCInterpreter.hpp"

using namespace bitty;

class ThrowingErrorListener : public antlr4::BaseErrorListener {
 public:
  void syntaxError(antlr4::Recognizer*, antlr4::Token*, size_t line, size_t col,
                   const std::string& msg, std::exception_ptr) override {
    std::ostringstream oss;
    oss << "syntax error at " << line << ":" << col << " - " << msg;
    throw std::runtime_error(oss.str());
  }
};

static std::string readFile(const std::string& path) {
  std::ifstream ifs(path, std::ios::binary);
  if (!ifs) {
    throw std::runtime_error("failed to open file: " + path);
  }
  std::ostringstream buf;
  buf << ifs.rdbuf();
  return buf.str();
}

static AST::Program parseToAST(const std::string& src) {
  antlr4::ANTLRInputStream input(src);
  bitty::parser::BittyLexer lexer(&input);
  ThrowingErrorListener err;
  lexer.removeErrorListeners();
  lexer.addErrorListener(&err);

  antlr4::CommonTokenStream tokens(&lexer);

  bitty::parser::BittyParser parser(&tokens);
  parser.removeErrorListeners();
  parser.addErrorListener(&err);

  auto* programCtx = parser.program();

  ASTBuilder ab;
  return ab.build(programCtx);
}

static void printUsage(const char* prog) {
  std::cerr << "Usage: " << prog
            << " [--ast | --il | --bc | --run] <source.bitty>\n"
            << "  --ast  Print AST pretty-printed Bitty source (default)\n"
            << "  --il   Lower to IL and print the IL pretty-printed source\n"
            << "  --bc   Lower to Bytecode and print the Bytecode "
               "pretty-printed source\n"
            << "  --run  Execute the program by AST interpreter\n";
}

int main(int argc, char** argv) {
  if (argc < 2) {
    printUsage(argv[0]);
    return 1;
  }
  enum Mode { EMPTY, IL, TYPEDIL, AST, BC, RUN, BCRUN };
  Mode mode = Mode::EMPTY;
  bool constantPropagation = false;
  bool optimize = false;
  std::string inputPath;

  for (int i = 1; i < argc; ++i) {
    std::string arg = argv[i];
    if (arg == "--help" || arg == "-h") {
      printUsage(argv[0]);
      return 0;
    } else if (arg == "--il") {
      mode = Mode::IL;
    } else if (arg == "--cp") {
      constantPropagation = true;
    } else if (arg == "--opt") {
      optimize = true;
    } else if (arg == "--typedil") {
      mode = Mode::TYPEDIL;
    } else if (arg == "--bc") {
      mode = Mode::BC;
    } else if (arg == "--bcrun") {
      mode = Mode::BCRUN;
    } else if (arg == "--ast") {
      mode = Mode::AST;
    } else if (arg == "--run") {
      mode = Mode::RUN;
    } else if (!arg.empty() && arg[0] == '-') {
      std::cerr << "Unknown option: " << arg << "\n";
      printUsage(argv[0]);
      return 1;
    } else {
      if (!inputPath.empty()) {
        std::cerr << "Multiple input files given. Only one is supported.\n";
        printUsage(argv[0]);
        return 1;
      }
      inputPath = arg;
    }
  }

  if (inputPath.empty()) {
    printUsage(argv[0]);
    return 1;
  }

  try {
    const std::string src = readFile(inputPath);
    AST::Program ast = parseToAST(src);

    if (mode == Mode::AST) {
      std::string pretty = AST::toSource(ast);
      std::cout << pretty << std::flush;
    } else if (mode == Mode::IL) {
      IL::Program il = IL::lowerFromAST(ast);
      if (constantPropagation) {
        IL::propagateConstants(il);
      }
      if (optimize) {
        IL::optimizeProgram(il);
      }
      std::string ilSource = IL::toSource(il);
      std::cout << ilSource << std::flush;
    } else if (mode == Mode::TYPEDIL) {
      IL::Program il = IL::lowerFromAST(ast);
      IL::inferTypes(il);
      if (constantPropagation) {
        IL::propagateConstants(il);
      }
      if (optimize) {
        IL::optimizeProgram(il);
      }
      std::string ilSource = IL::toSource(il);
      std::cout << ilSource << std::flush;
    } else if (mode == Mode::BC) {
      IL::Program il = IL::lowerFromAST(ast);
      if (constantPropagation) {
        IL::propagateConstants(il);
      }
      if (optimize) {
        IL::optimizeProgram(il);
      }
      BC::Module mod = BC::compileFromIL(il);
      std::cout << BC::disassemble(mod) << std::flush;
    } else if (mode == Mode::BCRUN) {
      IL::Program il = IL::lowerFromAST(ast);
      if (constantPropagation) {
        IL::propagateConstants(il);
      }
      if (optimize) {
        IL::optimizeProgram(il);
      }
      BC::Module mod = BC::compileFromIL(il);
      ExecResult r = BC::runBytecode(mod);
      if (!r.ok) {
        std::cerr << r.error << "\n";
      } else {
        std::cout << r.stdout_text;
      }
    } else if (mode == Mode::RUN) {
      auto r = interpret(ast);
      if (!r.ok) {
        std::cout << r.error << std::endl;
      }
      std::cout << r.stdout_text << std::endl;
    }
  } catch (const std::exception& e) {
    std::cerr << "[error] " << e.what() << "\n";
    return 2;
  }
  return 0;
}
