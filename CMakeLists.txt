cmake_minimum_required(VERSION 3.16)
project(Bitty VERSION 0.1.0 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)

option(BITTY_FETCH_DEPENDENCIES "Download dependencies with FetchContent" ON)
option(BITTY_BUILD_TESTS "Build unit tests" ON)
option(BITTY_PACKAGING "Generate installable CMake package (export targets)" OFF)

include(FetchContent)

find_package(Java COMPONENTS Runtime REQUIRED)
if(Java_VERSION_STRING VERSION_LESS "11")
  message(FATAL_ERROR
    "ANTLR >= 4.11 requires Java 11+. Found ${Java_VERSION_STRING} at ${Java_JAVA_EXECUTABLE}.")
endif()

set(ANTLR4_VERSION 4.13.2)
set(ANTLR_JAR "${CMAKE_BINARY_DIR}/antlr-${ANTLR4_VERSION}-complete.jar")
if(NOT EXISTS "${ANTLR_JAR}")
  file(DOWNLOAD
    "https://www.antlr.org/download/antlr-${ANTLR4_VERSION}-complete.jar"
    "${ANTLR_JAR}" SHOW_PROGRESS)
endif()
set(ANTLR_EXECUTABLE "${ANTLR_JAR}" CACHE FILEPATH "" FORCE)

set(_LOCAL_FINDANTLR "${PROJECT_SOURCE_DIR}/cmake/FindANTLR.cmake")
if(EXISTS "${_LOCAL_FINDANTLR}")
  list(APPEND CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/cmake")
else()
  set(_DL_DIR "${CMAKE_BINARY_DIR}/cmake")
  file(MAKE_DIRECTORY "${_DL_DIR}")
  set(_DOWNLOADED_FINDANTLR "${_DL_DIR}/FindANTLR.cmake")
  if(NOT EXISTS "${_DOWNLOADED_FINDANTLR}")
    file(DOWNLOAD
      "https://raw.githubusercontent.com/antlr/antlr4/${ANTLR4_VERSION}/runtime/Cpp/cmake/FindANTLR.cmake"
      "${_DOWNLOADED_FINDANTLR}" SHOW_PROGRESS)
  endif()
  list(APPEND CMAKE_MODULE_PATH "${_DL_DIR}")
endif()
include(FindANTLR)

if(BITTY_PACKAGING AND BITTY_FETCH_DEPENDENCIES)
  message(FATAL_ERROR
    "If BITTY_PACKAGING=ON, it must be BITTY_FETCH_DEPENDENCIES=OFF.")
endif()

if(BITTY_FETCH_DEPENDENCIES OR BITTY_GTEST_FETCH)
  FetchContent_Declare(
    googletest
    GIT_REPOSITORY https://github.com/google/googletest.git
    GIT_TAG        v1.14.0
  )
  set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
  FetchContent_MakeAvailable(googletest)
else()
  if(BITTY_BUILD_TESTS)
    find_package(googletest CONFIG REQUIRED)
  endif()
endif()


if(BITTY_FETCH_DEPENDENCIES OR BITTY_ANTLR_FETCH)
  FetchContent_Declare(
    antlr4_runtime
    GIT_REPOSITORY https://github.com/antlr/antlr4.git
    GIT_TAG        ${ANTLR4_VERSION}
    SOURCE_SUBDIR  runtime/Cpp
  )
  set(WITH_DEMO OFF CACHE BOOL "" FORCE)
  set(ANTLR4_INSTALL OFF CACHE BOOL "" FORCE)
  FetchContent_MakeAvailable(antlr4_runtime)
else()
  find_package(antlr4-runtime REQUIRED)
endif()

include(CheckIPOSupported)
check_ipo_supported(RESULT _ipo_ok OUTPUT _ipo_msg)
if(_ipo_ok)
  set(CMAKE_INTERPROCEDURAL_OPTIMIZATION_RELEASE ON)
endif()

add_subdirectory(src)

if(BITTY_PACKAGING)
  include(GNUInstallDirs)
  include(CMakePackageConfigHelpers)

  install(TARGETS parser core
    EXPORT BittyTargets
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/bitty/core
  )
  install(TARGETS cli
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  )

  install(EXPORT BittyTargets
    NAMESPACE bitty::
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/Bitty
  )

  configure_package_config_file(
    "${PROJECT_SOURCE_DIR}/cmake/BittyConfig.cmake.in"
    "${CMAKE_CURRENT_BINARY_DIR}/BittyConfig.cmake"
    INSTALL_DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/Bitty
  )
  write_basic_package_version_file(
    "${CMAKE_CURRENT_BINARY_DIR}/BittyConfigVersion.cmake"
    VERSION ${PROJECT_VERSION}
    COMPATIBILITY SameMajorVersion
  )
  install(FILES
    "${CMAKE_CURRENT_BINARY_DIR}/BittyConfig.cmake"
    "${CMAKE_CURRENT_BINARY_DIR}/BittyConfigVersion.cmake"
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/Bitty
  )
endif()

include(CTest)
if (BITTY_BUILD_TESTS)
  enable_testing()

  add_executable(BittyCaseTests
    tests/CaseTests.cpp
    tests/ILLoweringTest.cpp
    tests/ConstantPropagationTest.cpp
    tests/ASTTest.cpp
    tests/utils/ILVerify.cpp
    tests/utils/TestUtils.hpp
    tests/utils/ASTEqual.hpp
    tests/InterpreterTest.cpp
  )
  target_include_directories(BittyCaseTests PRIVATE
    ${CMAKE_SOURCE_DIR}/tests/utils
  )
  target_compile_definitions(BittyCaseTests PRIVATE
    BITTY_SOURCE_DIR="${CMAKE_SOURCE_DIR}"
  )
  target_link_libraries(BittyCaseTests PRIVATE
    core parser ${ANTLR4_RUNTIME_TARGET} gtest_main
  )

  include(GoogleTest)
  gtest_discover_tests(BittyCaseTests
    DISCOVERY_TIMEOUT 60
  )
endif()
