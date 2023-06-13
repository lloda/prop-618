;; (prop.scm) -*- coding: utf-8; mode: scheme -*-
;; generate bindings from Fortran module.

;; (c) lloda@sarc.name 2019, 2021
;; This library is free software; you can redistribute it and/or modify it under
;; the terms of the GNU Lesser General Public License as published by the Free
;; Software Foundation; either version 3 of the License, or (at your option) any
;; later version.

;; This is a hack and not a parser, so a fairly rigid format is required from
;; the .f90 declarations. Try to complain if we don't understand, so that the
;; declarations are not missed and can be fixed.

(import (srfi :1) (srfi :2) (srfi :8) (srfi :26) (srfi :19)
        (ice-9 rdelim) (ice-9 format) (system foreign) (ice-9 match)
        (ice-9 pretty-print))

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
                  (isep (and tokens (and line (list-index (cut eq? <> '::) tokens)))))
             (receive (pre post) (if isep (split-at tokens isep) (values #f #f))
               (let ((post (and post (not (null? post)) (cdr post))))
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
                        (loop (+ 1 i) dict))))))))))

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
       (define (parse-bind bind-arg . extra)
         (unless (or (equal? extra '()) (equal? extra '(&)))
           (throw 'unexpected-extra-symbols-in-bind-line extra bind-line))
         (let ((bind-name (and (>= (string-length bind-arg) 7)
                               (string=? "name='" (string-take bind-arg 6))
                               (string=? "'" (string-take-right bind-arg 1))
                               (string-drop-right (string-drop bind-arg 6) 1))))
           (match tokens
             ((fortran-type (c-type) 'function name (. args) '&)
              (receive (i dict) (parse-args o (+ 1 i) args)
                (values i (list c-type (or bind-name (symbol->string name)) dict))))
             (('subroutine name (. args) '&)
              (receive (i dict) (parse-args o (+ 1 i) args)
                (values i (list 'void (or bind-name (symbol->string name)) dict))))
             (x (values (+ 1 i) #f)))))
; FIXME should parse as comma-separated list
       (match extra
         ((('c, bind-arg) extra ...)
          (apply parse-bind (symbol->string bind-arg) extra))
         ((('c) extra ...)
          (apply parse-bind "" extra))
         (x (throw 'cannot-parse-bind-line bind-line))))
      (x (values (+ 1 i) #f)))))

(define (find-foreign-protos fname)
  (call-with-input-file fname
    (lambda (o)
      (let loop ((i 1) (xpts '()))
        (let ((line (read-line-w/o-comment o)))
          (cond ((not line) (reverse! xpts))
                ((string-contains line "subroutine")
                 (receive (i def) (parse-header o 'subroutine i line)
                   (loop (+ 1 i) (if def (cons def xpts) xpts))))
                ((string-contains line "function")
                 (receive (i def) (parse-header o 'function i line)
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
      (format o "\n// This file, ~a, has been generated from ~a by protos.scm\n"
              (basename dest) tag)
      (format o "
#pragma once
#include <stdint.h>
#ifdef __cplusplus
extern \"C\" {
#endif
")
      (for-each
          (match-lambda
            ((type bind-name ((aname atype inout . x) ...))
             (format o "\n~a\n~a\n(~{~{~a ~a * ~a~}~^, ~});\n"
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
      (format o "
#ifdef __cplusplus
} // extern \"C\"
#endif

// end of ~a

" (basename dest)))))

(define (write-bindings-python xpts tag libname dest)

  (define (c-type type)
    (match type
      ('void 'None)
      ('C_DOUBLE 'c_double)
      ('C_INT32_T 'c_int32)))

  (call-with-output-file dest
    (lambda (o)
      (format o "\n# This file, ~a, has been generated from ~a by protos.scm\n"
              (basename dest) tag)
      (format o "
import ctypes
from ctypes import c_int32, c_double, byref
from ctypes.util import find_library
liba = ctypes.cdll.LoadLibrary(find_library('~a'))
ec = liba.~a_init()
if ec!=0:
    raise RuntimeError('prop-618 could not be initialized (error: {})'.format(ec))
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

(define (write-bindings-guile xpts tag libname dest)

  (define (c-type type)
    (match type
      ('void 'void)
      ('C_DOUBLE 'double)
      ('C_INT32_T 'int32)))

  (define (v-type type)
    (match type
      ('C_DOUBLE 'f64vector)
      ('C_INT32_T 's32vector)))

  (define (make-pname aname)
    (string->symbol (format #f "p_~a" aname)))

  (define (make-sname bind-name)
    (string->symbol (string-map (lambda (c) (if (char=? c #\_) #\- c)) bind-name)))

  (define (make-iname bind-name)
    (string->symbol (format #f "__~a" bind-name)))

  (call-with-output-file dest
    (lambda (o)
      (format o "\n;;; This file, ~a, has been generated from ~a by protos.scm\n\n"
              (basename dest) tag)
      (for-each (lambda (e) (pretty-print e o))
; FIXME fix module name and library path at install
        `((define-module (prop-618 ,(string->symbol libname))
            #:export ,(match xpts
                        (((rtype bind-name x ...) ...)
                         (append (map make-sname bind-name)
                                 (map make-iname bind-name)))))
          (import (srfi :1) (system foreign) (rnrs bytevectors))
          (define liba
            (dynamic-link ,(format #f "lib~a" libname)))
          (define-syntax %callp
            (syntax-rules () ((_ f x ...) (f (bytevector->pointer x 0) ...))))
          (define-syntax %valuesp
            (syntax-rules () ((_ x ...) (values (array-ref x 0) ...))))))
      (newline o)
      (for-each (match-lambda
                  ((internal external)
                   (write internal o) (newline o)
                   (pretty-print external o) (newline o)))
        (map
         (match-lambda
           ((rtype bind-name ((aname atype inout . x) ...))
            (let ((direct-name (make-iname bind-name))
                  (pname (map make-pname aname)))
              `((define ,direct-name
                  (pointer->procedure
                   ,(c-type rtype)
                   (dynamic-func ,bind-name liba)
                   (quote ,(make-list (length aname) '*))))
                (define (,(if (string=? bind-name (format #f "~a_init" libname))
                            'init
                            (make-sname bind-name))
                         ,@(filter-map (lambda (aname inout)
                                         (match inout
                                           ((or 'in 'inout) aname)
                                           (x #f)))
                             aname inout))
                  (let ,(map (lambda (pname aname atype inout)
                               `(,pname
                                 (,(v-type atype)
                                  ,(match inout
                                     ((or 'in 'inout) aname)
                                     (x 0)))))
                          pname aname atype inout)
                    ,@(let* ((fcall `(%callp ,direct-name ,@pname))
                             (results (append
                                       (if (eq? rtype 'void) '() '(result))
                                       (filter-map (lambda (pname inout)
                                                     (match inout
                                                       ((or 'out 'inout) pname)
                                                       (x #f)))
                                         pname inout)))
                             (nresults (length results)))
                        (cond ((zero? nresults)
                               (pk 'not-sure-this-is-right bind-name)
                               `(,fcall))
                              ((= 1 nresults)
                               (if (eq? rtype 'void)
                                 `(,fcall ,(first results))
                                `(,fcall)))
                              ((eq? rtype 'void)
                               `(,fcall (%valuesp ,@results)))
                              (else
                               `((let ((result ,fcall)) (%valuesp ,@results))))))))))))
         xpts))
      (pretty-print '(let ((ec (init))) (unless (zero? ec) (throw 'prop-618-couldnt-be-initialized ec))) o)
      (newline o)
      (format o "\n; end of ~a\n\n" (basename dest)))))

(match (program-arguments)
  ((me libname dest source)
   ((cond ((string-suffix? ".h" dest)
           write-bindings-c)
          ((string-suffix? ".py" dest)
           write-bindings-python)
          ((string-suffix? ".scm" dest)
           write-bindings-guile)
          (else (throw 'cannot-write-bindings-to dest)))
    (find-foreign-protos source) (basename source) libname dest))
  (x (throw 'expected-arguments-1-source-2-dest)))
