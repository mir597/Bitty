antlr_target(
  BittyParser
  ${PROJECT_SOURCE_DIR}/grammar/Bitty.g4
  PACKAGE bitty::parser
  VISITOR
)

add_library(parser STATIC ${ANTLR_BittyParser_CXX_OUTPUTS})
add_library(bitty::parser ALIAS parser)

target_include_directories(parser
  PUBLIC
    $<BUILD_INTERFACE:${ANTLR_BittyParser_OUTPUT_DIR}>
    $<INSTALL_INTERFACE:include/bitty/parser>
)
target_compile_definitions(parser PUBLIC ANTLR4CPP_STATIC)

target_link_libraries(parser PUBLIC antlr4_static)

add_library(core STATIC
  core/AST.cpp
  core/ASTBuilder.cpp
)
target_sources(core PRIVATE
  core/Lowering.cpp
  core/IL.cpp
  core/Interpreter.cpp
  core/Bytecode.cpp
  core/IL2Bytecode.cpp
  core/TypeInference.cpp
  core/ConstantPropagation.cpp
  core/BCInterpreter.cpp  
  core/LiteralValue.cpp
  core/ILOptimize.cpp
)

add_library(bitty::core ALIAS core)

set_target_properties(core PROPERTIES
  PUBLIC_HEADER "core/AST.hpp;core/ASTBuilder.hpp"
)

target_include_directories(core
  PUBLIC
    $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/src>
    $<INSTALL_INTERFACE:include>
    $<BUILD_INTERFACE:${ANTLR_BittyParser_OUTPUT_DIR}>
    $<INSTALL_INTERFACE:include/bitty/parser>
)

target_link_libraries(core
  PUBLIC
    bitty::parser
)

target_compile_definitions(core PUBLIC ANTLR4CPP_STATIC)

target_compile_options(core   PRIVATE -Wall -Wextra -Wpedantic)
target_compile_options(parser PRIVATE -Wall -Wextra -Wpedantic)

add_executable(cli cli/Main.cpp)
target_link_libraries(cli PRIVATE bitty::core)

include(GNUInstallDirs)
install(
  DIRECTORY ${ANTLR_BittyParser_OUTPUT_DIR}/
  DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/bitty/parser
  FILES_MATCHING PATTERN "*.h" PATTERN "*.hpp"
)
