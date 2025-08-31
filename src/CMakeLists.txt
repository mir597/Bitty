# Parser
antlr_target(
  BittyParser
  ${PROJECT_SOURCE_DIR}/grammar/Bitty.g4
  PACKAGE bitty::parser
  VISITOR
)

# Common
add_library(common STATIC
  core/AST.cpp
  core/IL.cpp
  core/Bytecode.cpp
  core/LiteralValue.cpp
)
add_library(bitty::common ALIAS common)

target_include_directories(common
  PUBLIC
    $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/src>
    $<INSTALL_INTERFACE:include>
)

target_compile_options(common PRIVATE -Wall -Wextra -Wpedantic)

# Parser + AST Builder
add_library(frontend STATIC
  ${ANTLR_BittyParser_CXX_OUTPUTS}
  core/ASTBuilder.cpp
)
add_library(bitty::frontend ALIAS frontend)

target_include_directories(frontend
  PUBLIC
    $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/src>
    $<BUILD_INTERFACE:${ANTLR_BittyParser_OUTPUT_DIR}>
    $<INSTALL_INTERFACE:include>
    $<INSTALL_INTERFACE:include/bitty/parser>
)

target_link_libraries(frontend
  PUBLIC
    bitty::common
    antlr4_static
)

target_compile_definitions(frontend PUBLIC ANTLR4CPP_STATIC)
target_compile_options(frontend PRIVATE -Wall -Wextra -Wpedantic)

# AST Interpreter
add_library(ast_interpreter STATIC
  core/Interpreter.cpp
)
add_library(bitty::ast_interpreter ALIAS ast_interpreter)

target_link_libraries(ast_interpreter PUBLIC bitty::common)
target_include_directories(ast_interpreter
  PUBLIC
    $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/src>
    $<INSTALL_INTERFACE:include>
)
target_compile_options(ast_interpreter PRIVATE -Wall -Wextra -Wpedantic)

# IL Translation/Optimization + BC Translation
add_library(compiler STATIC
  core/Lowering.cpp
  core/TypeInference.cpp
  core/ConstantPropagation.cpp
  core/ILOptimize.cpp
  core/IL2Bytecode.cpp
)
add_library(bitty::compiler ALIAS compiler)

target_link_libraries(compiler PUBLIC bitty::common)
target_include_directories(compiler
  PUBLIC
    $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/src>
    $<INSTALL_INTERFACE:include>
)
target_compile_options(compiler PRIVATE -Wall -Wextra -Wpedantic)

# BC Interpreter
add_library(bc_interpreter STATIC
  core/BCInterpreter.cpp
)
add_library(bitty::bc_interpreter ALIAS bc_interpreter)

target_link_libraries(bc_interpreter PUBLIC bitty::common)
target_include_directories(bc_interpreter
  PUBLIC
    $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/src>
    $<INSTALL_INTERFACE:include>
)
target_compile_options(bc_interpreter PRIVATE -Wall -Wextra -Wpedantic)

# Executable
add_executable(cli cli/Main.cpp)
target_link_libraries(cli PRIVATE
  bitty::frontend
  bitty::ast_interpreter
  bitty::compiler
  bitty::bc_interpreter
)

# headers
include(GNUInstallDirs)

install(
  DIRECTORY ${PROJECT_SOURCE_DIR}/src/core/
  DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/bitty/core
  FILES_MATCHING PATTERN "*.h" PATTERN "*.hpp"
)

install(
  DIRECTORY ${ANTLR_BittyParser_OUTPUT_DIR}/
  DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/bitty/parser
  FILES_MATCHING PATTERN "*.h" PATTERN "*.hpp"
)