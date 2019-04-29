;; (prop.scm) -*- coding: utf-8; mode: scheme-mode -*-
;; generate bindings from Fortran module.

;; (c) lloda@sarc.name 2019
;; This library is free software; you can redistribute it and/or modify it under
;; the terms of the GNU Lesser General Public License as published by the Free
;; Software Foundation; either version 3 of the License, or (at your option) any
;; later version.

;; This is a hack and not a parser, so a fairly rigid format is required from
;; the .f95 declarations. Try to complain if we don't understand, so that the
;; declarations are not missed and can be fixed.

(import (srfi :1) (srfi :2) (srfi :8) (srfi :26) (srfi :71) (srfi :19)
        (ice-9 rdelim) (ice-9 format) (system foreign) (ice-9 match) (rnrs bytevectors))

(define (read-line-w/o-comment o)
  (let ((line (read-delimited "\n" o)))
    (if (eof-object? line)
      #f
      (let ((line (call-with-input-string line (cut read-delimited "!" <>))))
        (if (eof-object? line) "" line)))))

(define (read-list port)
  (reverse! (unfold-right eof-object? values (lambda (x) (read port)) (read port))))

; tokens are from  parse-header, and we've checked the bind-line. Try to get the arg types.
(define (parse-args o i args)
  (let loop ((i i) (dict (map (cut cons <> #f) args))) ; initialize so we'll have to match args in dict.
    (cond ((every cdr dict)
           (unless (= (length dict) (length args))
             (throw 'unexpected-arg-names args dict))
           (values i dict))
          (else
           (let* ((line (read-line-w/o-comment o))
                  (line (and line (string-map (match-lambda (#\, #\space) (x x)) line)))
                  (tokens (and line (call-with-input-string line read-list)))
                  (isep (and tokens (and line (list-index (cut eq? <> '::) tokens))))
                  ((values pre post) (if isep (split-at tokens isep) (values #f #f)))
                  (post (and post (not (null? post)) (cdr post))))
             (cond ((and tokens (not isep))
                    (match tokens
                      (() (loop (+ 1 i) dict))            ; blank
                      (('result . x) (loop (+ 1 i) dict)) ; result spec which we don't use
                      (x (throw 'expecting-arg-type-declaration i 'tokens tokens 'line line))))
                   ((and isep (not post))
                    (throw 'expecting-vars-of-type i line))
                   (pre
                    (match (list pre post)
                      (((fortran-type (c-type) stuff ...) (vars ...))
                       (when (list-index (cut eq? <> 'dimension) stuff)
                         (throw 'dimension-not-supported i line))
                       (let* ((iinout (list-index (cut eq? <> 'intent) stuff))
                              (inout (if iinout
                                       (match (drop stuff iinout)
                                         (('intent (inout) . x) inout)
                                         (x (throw 'bad-inout-clause x)))
                                       'inout)))
                         (loop (+ 1 i)
                               (fold (lambda (var dict) (assq-set! dict var (list c-type inout))) dict vars))))
                      (x (throw 'some-args-types-missing-before-this-line i line))))
                   (else
                    (loop (+ 1 i) dict))))))))

; line is a function or subroutine line. Try to get bind and the args.
(define (parse-header o type i line)
  (let* ((line (string-map (match-lambda (#\, #\space) (x x)) line))
         (tokens (call-with-input-string line read-list))
         (bind-line (and (not (equal? 'end (car tokens))) (read-line-w/o-comment o)))
         (bind-tokens (and bind-line (call-with-input-string bind-line read-list))))
    (match bind-tokens
      (#f
       (values (+ 1 i) #f))
      (('bind extra ...)
; FIXME this only matches bind='...', with single quotes. Shouldn't try to parse the () as a list.
       (match extra
         ((('c, bind-arg) extra ...)
          (unless (or (equal? extra '()) (equal? extra '(&)))
            (throw 'unexpected-extra-symbols-in-bind-line extra bind-line))
          (let* ((a (symbol->string bind-arg)))
            (if (or (< (string-length a) 7)
                    (not (string=? "name='" (string-take a 6)))
                    (not (string=? "'" (string-take-right a 1))))
              (throw 'cannot-determine-bind-name a)
              (let ((bind-name (string-drop-right (string-drop a 6) 1)))
                (match tokens
                  ((fortran-type (c-type) 'function name (. args) '&)
                   (let (((values i dict) (parse-args o (+ 1 i) args)))
                     (values i (list c-type bind-name dict))))
                  (('subroutine name (. args) '&)
                   (let (((values i dict) (parse-args o (+ 1 i) args)))
                     (values i (list 'void bind-name dict))))
                  (x (values (+ 1 i) #f)))))))
         (x (throw 'cannot-parse-bind-line bind-line))))
      (x (values (+ 1 i) #f)))))

(define (find-foreign-protos fname)
  (call-with-input-file fname
    (lambda (o)
      (let loop ((i 1) (xpts '()))
        (let ((line (read-line-w/o-comment o)))
          (cond ((not line) (reverse! xpts))
                ((string-contains line "subroutine")
                 (let (((values i def) (parse-header o 'subroutine i line)))
                   (loop (+ 1 i) (if def (cons def xpts) xpts))))
                ((string-contains line "function")
                 (let (((values i def) (parse-header o 'function i line)))
                   (loop (+ 1 i) (if def (cons def xpts) xpts))))
                (else
                 (loop (+ 1 i) xpts))))))))

; FIXME Generate Python bindings (cffi?)
; FIXME Generate Guile bindings, replacing prop.scm

(define (write-bindings-c xpts tag libname dest)

  (define (c-type type)
    (match type
      ('void 'void)
      ('C_DOUBLE 'double)
      ('C_INT32_T 'int32_t)))

  (call-with-output-file dest
    (lambda (o)
      (format o "\n// ~a generated from ~a by protos.scm\n" (basename dest) tag)
      (format o "
#pragma once
#include <stdint.h>
#ifdef __cplusplus
extern \"C\" {
#endif
\n")
      (for-each
       (match-lambda
         ((type bind-name ((aname atype inout . x) ...))
          (format o "~a\n~a\n(~{~{~a ~a * ~a~}~^, ~});\n\n"
                  (c-type type)
                  bind-name
                  (zip (map c-type atype)
                       (map (match-lambda
                              ('in 'const)
                              ('out "")
                              ('inout "/* inout */")
                              (x (throw 'bad-inout-tag x)))
                         inout)
                       aname))))
       xpts)
      (format o "#ifdef __cplusplus\n")
      (format o "} // extern \"C\"\n")
      (format o "#endif\n\n")
      (format o "// end of ~a\n\n" (basename dest)))))

(define (write-bindings-python xpts tag libname dest)

  (define (c-type type)
    (match type
      ('void 'None)
      ('C_DOUBLE 'c_double)
      ('C_INT32_T 'c_int32)))

  (call-with-output-file dest
    (lambda (o)
      (format o "\n# ~a generated from ~a by protos.scm\n" (basename dest) tag)
      (format o "
import ctypes
from ctypes import c_double, c_int32, POINTER, byref
from ctypes.util import find_library
liba = ctypes.cdll.LoadLibrary(find_library('~a'))
liba.~a_init()
\n"
              libname libname)

      (for-each
       (match-lambda
         ((rtype bind-name ((aname atype inout . x) ...))

          (format o "def ~a(~{~a~^, ~}):\n"
; special case for module init function
                  (if (string=? bind-name (format #f "~a_init" libname))
                    "init"
                    bind-name)
                  (filter-map
                      (lambda (aname inout)
                        (match inout
                          ((or 'in 'inout) aname)
                          (x #f)))
                    aname inout))
          (for-each
           (lambda (aname atype inout)
             (format o "    p_~a = ~a(~a)\n"
                     aname (c-type atype)
                     (match inout
                       ((or 'in 'inout) aname)
                       (x "0"))))
           aname atype inout)

          (let* ((rtype (c-type rtype))
                 (inreturn
                  (match rtype
                    ('None
                     (format o "    liba.~a(~{byref(p_~a)~^, ~})\n"
                             bind-name aname)
                     '())
                    (rtype
                     (format o "    liba.~a.restype = ~a\n" bind-name rtype)
                     (format o "    result_ = liba.~a(~{byref(p_~a)~^, ~})\n"
                             bind-name aname)
                     '(result_)))))
            (format o "    return ~{~a~^, \\\n           ~}\n\n"
                    (append inreturn
                            (filter-map
                                (lambda (aname inout)
                                  (match inout
                                    ((or 'out 'inout) (format #f "p_~a.value" aname))
                                    (x #f)))
                              aname inout))))))
       xpts)

      (format o "# end of ~a\n\n" (basename dest)))))

(match (program-arguments)
  ((me libname dest source)
   ((cond ((string-suffix? ".h" dest)
           write-bindings-c)
          ((string-suffix? ".py" dest)
           write-bindings-python)
          (else (throw 'cannot-write-bindings-to dest)))
    (find-foreign-protos source) (basename source) libname dest))
  (x (throw 'expected-arguments-1-source-2-dest)))
