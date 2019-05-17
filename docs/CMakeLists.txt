# -*- coding: utf-8; mode: cmake -*-
# CMakeLists.txt for top/docs folder.

cmake_minimum_required (VERSION 3.5)
project (prop-618-docs)

set (CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_CURRENT_SOURCE_DIR}/../config)
include(UseLATEX)
add_latex_document (manual.tex
  INPUTS prologue.tex margins.tex
  BIBFILES biblio.bib)

# TODO HTML output with latex
