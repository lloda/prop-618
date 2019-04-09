# -*- mode: cmake; coding: utf-8 -*-

# -------------------
# readme
# -------------------

# cd build FFLAGS=-O3 cmake ..
# VERBOSE=1 make -j7
# VERBOSE=1 make -j7 test

# -------------------
# prologue
# -------------------

project(e2s-prop-models)
enable_language(Fortran)
cmake_minimum_required (VERSION 3.5)
include_directories(".")
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/config/")

# -------------------
# the meat
# -------------------

set (BASE_FFLAGS " \
-std=f2008 -fbounds-check")
set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${BASE_FFLAGS} $ENV{FFLAGS}")
set (LINK_FLAGS "-fopenmp $ENV{LINKFLAGS}")

add_library (prop src/prop.f95)

# foreach (target one two)
#   add_executable (${target} "test/${target}.f95")
#   target_link_libraries (${target} asu m -static)
#   add_test (${target} ${target})
# endforeach ()

include(config/UseLATEX.cmake)
set (LATEX_OUTPUT_PATH build)
add_latex_document (docs/prop-doc.tex
  INPUTS docs/paper-prologue.tex docs/margins.tex
  BIBFILES docs/biblio.bib
  USE_INDEX
  USE_GLOSSARY)

enable_testing ()

message ("* Current build type is: ${CMAKE_BUILD_TYPE}")
message ("* Crosscompiling is: ${CMAKE_CROSSCOMPILING}")
message ("* Compiler is is: ${CMAKE_CXX_COMPILER}")
message ("* C++ compiler is: ${CMAKE_CXX_COMPILER}")
message ("* CXX_FLAGS are: ${CMAKE_CXX_FLAGS}")
message ("* Archive command is: ${CMAKE_AR}")
message ("* Current build dir is: ${CMAKE_BUILD_DIR}")
message ("* Current binary dir is: ${CMAKE_BINARY_DIR}")
