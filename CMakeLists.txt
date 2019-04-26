# -*- mode: cmake; coding: utf-8 -*-
# global CMakeLists.txt
# 2019-04

# (c) lloda@sarc.name
# This library is free software; you can redistribute it and/or modify it under
# the terms of the GNU Lesser General Public License as published by the Free
# Software Foundation; either version 3 of the License, or (at your option) any
# later version.

# -------------------
# readme
# -------------------

# cd build && FFLAGS=-O3 cmake ..
# VERBOSE=1 make && make test

# -------------------
# prologue
# -------------------

project (prop-618)
enable_language (Fortran)
cmake_minimum_required (VERSION 3.5)
include_directories (".")
set (CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/config/")

# -------------------
# body
# -------------------

set (BASE_FFLAGS " \
-std=f2018 -fdefault-real-8 -fbounds-check -fimplicit-none -fopenmp -fdiagnostics-color=always -Wall -Werror -ffpe-summary=invalid,overflow,zero")
set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${BASE_FFLAGS} $ENV{FFLAGS}")
set (LINK_FLAGS "-fopenmp $ENV{LINKFLAGS}")

add_library (prop SHARED src/prop.f95)
add_library (atmospheres SHARED src/atmospheres.f95)

foreach (target test0)
  add_executable (${target} "src/${target}.f95")
  target_link_libraries (${target} prop atmospheres)
  add_test (${target} ${target})
endforeach ()

# used to generate bindings
include (config/FindGuile.cmake)

# include(config/UseLATEX.cmake)
# set (LATEX_OUTPUT_PATH build)
# add_latex_document (docs/prop-doc.tex
#   INPUTS docs/paper-prologue.tex docs/margins.tex
#   # USE_BIBLATEX # problems with biblatex & biber
#   BIBFILES docs/biblio.bib
#   # USE_INDEX
#   # USE_GLOSSARY
#   )

enable_testing ()

message ("* Current build type is: ${CMAKE_BUILD_TYPE}")
message ("* Crosscompiling is: ${CMAKE_CROSSCOMPILING}")
message ("* C++ compiler is: ${CMAKE_CXX_COMPILER}")
message ("* Fortran compiler is: ${CMAKE_Fortran_COMPILER}")
message ("* Fortran_FLAGS are: ${CMAKE_Fortran_FLAGS}")
message ("* CXX_FLAGS are: ${CMAKE_CXX_FLAGS}")
message ("* Archive command is: ${CMAKE_AR}")
message ("* Current build dir is: ${CMAKE_BUILD_DIR}")
message ("* Current binary dir is: ${CMAKE_BINARY_DIR}")
