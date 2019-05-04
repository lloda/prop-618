# -*- coding: utf-8; mode: cmake -*-
# global CMakeLists.txt

# (c) lloda@sarc.name 2019
# This library is free software; you can redistribute it and/or modify it under
# the terms of the GNU Lesser General Public License as published by the Free
# Software Foundation; either version 3 of the License, or (at your option) any
# later version.

# -------------------
# readme
# -------------------

# cd build && FFLAGS=-O3 cmake -DCMAKE_INSTALL_PREFIX=./install .. && VERBOSE=1 make && make test && cmake

# -------------------
# prologue
# -------------------

project (prop_618)
enable_language (Fortran C)
cmake_minimum_required (VERSION 3.5)
include_directories (${CMAKE_SOURCE_DIR}/mod)
set (CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_SOURCE_DIR}/config)

# -------------------
# body
# -------------------

set (BASE_FFLAGS "-std=f2018 -fopenmp -fdiagnostics-color=always -Wall -Werror \
-fdefault-real-8 -fbounds-check -fimplicit-none -ffpe-summary=invalid,overflow,zero")
set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${BASE_FFLAGS} $ENV{FFLAGS}")

set (BASE_CLAGS " -std=c17 -fopenmp -fdiagnostics-color=always -Wall -Werror")
set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${BASE_CFLAGS} $ENV{CFLAGS}")

set (LINK_FLAGS "-fopenmp $ENV{LINKFLAGS}")

include (GNUInstallDirs)
set (PROP_DATADIR "${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_DATADIR}/prop-618/")
set (PROP_TEST_DATADIR "${CMAKE_SOURCE_DIR}/data/")
configure_file (src/config.f95.in config.f95)
add_library (prop SHARED src/prop.f95 config.f95)
add_library (atmospheres SHARED src/atmospheres.f95 config.f95)

# -------------------
# bindings
# -------------------

# If Guile isn't available, will rely on committed bindings in mod/.

find_package (Guile)
if (GUILE_EXECUTABLE)
  add_custom_target (bindings ALL)
  foreach (libname atmospheres prop)
    set (source "${CMAKE_SOURCE_DIR}/src/${libname}.f95")
    set (generator "${CMAKE_SOURCE_DIR}/config/protos.scm")
    set (bindc "${CMAKE_SOURCE_DIR}/mod/${libname}.h")
    set (bindpython "${CMAKE_SOURCE_DIR}/mod/prop_618/${libname}.py")
    set (bindguile "${CMAKE_SOURCE_DIR}/mod/prop-618/${libname}.scm")
    foreach (dest ${bindc} ${bindpython} ${bindguile})
      get_filename_component (dest_as_target ${dest} NAME)
      add_custom_target (${dest_as_target}
        DEPENDS ${source} ${generator}
        COMMAND ${GUILE_EXECUTABLE} -s ${generator} ${libname} ${dest} ${source}
        BYPRODUCTS ${dest}
        COMMENT "Generating binding file ${dest}"
        VERBATIM)
      add_dependencies (bindings ${dest_as_target})
    endforeach ()
  endforeach ()
endif ()

# -------------------
# tests
# -------------------

# Fortran, this has the functional tests.

foreach (target test-0)
  add_executable (${target} "test/${target}.f95")
  target_link_libraries (${target} prop atmospheres)
  add_test (${target} ${target})
endforeach ()

# C, just the headers.

foreach (target test-1)
  add_executable (${target} "test/${target}.c"
    ${CMAKE_SOURCE_DIR}/mod/atmospheres.h
    ${CMAKE_SOURCE_DIR}/mod/prop.h)
  target_link_libraries (${target} prop atmospheres)
  add_test (${target} ${target})
endforeach ()

# Python, just the bindings.

find_package (Python3)
if (Python3_FOUND)
  foreach (target test-2)
# FIXME Would like a way to make these depend on mod/*.py
    add_test (${target} ${Python3_EXECUTABLE} "${CMAKE_SOURCE_DIR}/test/${target}.py")
    set_tests_properties (${target} PROPERTIES ENVIRONMENT "PYTHONPATH=${CMAKE_SOURCE_DIR}/mod")
  endforeach ()
endif ()

# Guile, just test the bindings.

if (GUILE_FOUND)
  foreach (target test-3)
# FIXME Would like a way to make these depend on mod/*.py
    add_test (${target} ${GUILE_EXECUTABLE} -q -L "${CMAKE_SOURCE_DIR}/mod" "${CMAKE_SOURCE_DIR}/test/${target}.scm")
  endforeach ()
endif ()

# -------------------
# documentation
# -------------------

# queries(config/UseLATEX.cmake)
# set (LATEX_OUTPUT_PATH build)
# add_latex_document (docs/prop-doc.tex
#   INPUTS docs/paper-prologue.tex docs/margins.tex
#   # USE_BIBLATEX # problems with biblatex & biber
#   BIBFILES docs/biblio.bib
#   # USE_INDEX
#   # USE_GLOSSARY
#   )

# -------------------
# install
# -------------------

install (DIRECTORY "${CMAKE_SOURCE_DIR}/data/"
  DESTINATION "${CMAKE_INSTALL_DATADIR}/prop-618")

foreach (module prop atmospheres)
  install (TARGETS ${module}
    DESTINATION lib)
  install (FILES "${CMAKE_CURRENT_BINARY_DIR}/${module}.mod"
    DESTINATION include)
  install (FILES "${CMAKE_SOURCE_DIR}/mod/${module}.h"
    DESTINATION include)
endforeach ()

if (GUILE_FOUND)
  foreach (module prop atmospheres)
    install (FILES "${CMAKE_SOURCE_DIR}/mod/prop-618/${module}.scm"
      DESTINATION "${CMAKE_INSTALL_DATADIR}/guile/site/prop-618")
  endforeach ()
endif ()

if (Python3_FOUND)
  execute_process(
    COMMAND "${Python3_EXECUTABLE}" -c "if True:
    from distutils import sysconfig as sc
    print(sc.get_python_lib(prefix='', plat_specific=True))"
    OUTPUT_VARIABLE Python3_SITE
    OUTPUT_STRIP_TRAILING_WHITESPACE)

  message ("* Python3 install site: ${Python3_SITE}")
  foreach (module prop atmospheres)
    install(FILES "${CMAKE_SOURCE_DIR}/mod/prop_618/${module}.py"
      DESTINATION "${Python3_SITE}/prop_618") # FIXME maybe I should just change this
  endforeach ()
endif ()

# -------------------
# epilogue
# -------------------

enable_testing ()

message ("* Current build type is: ${CMAKE_BUILD_TYPE}")
message ("* Crosscompiling is: ${CMAKE_CROSSCOMPILING}")
message ("* C compiler is: ${CMAKE_C_COMPILER}")
message ("* C flags are: ${CMAKE_C_FLAGS}")
message ("* C++ compiler is: ${CMAKE_CXX_COMPILER}")
message ("* C++ flags are: ${CMAKE_CXX_FLAGS}")
message ("* Fortran compiler is: ${CMAKE_Fortran_COMPILER}")
message ("* Fortran_FLAGS are: ${CMAKE_Fortran_FLAGS}")
message ("* Current binary dir is: ${CMAKE_BINARY_DIR}")
message ("* Install prefix is: ${CMAKE_INSTALL_PREFIX}")
