#lang racket

(require mzlib/foreign) (unsafe!)
(require racket/runtime-path)

; We need this because libz3 is stupid and doesn't mention a dependence on
; libgomp. Loading this causes libz3 to pick up libgomp and thus not error out.
(define libgomp (ffi-lib "libgomp" '["1" #f]))

(define-runtime-path libz3-path "z3/lib/libz3-gmp")
(define libz3 (ffi-lib libz3-path))

(define-cpointer-type _z3-config)
(define-cpointer-type _z3-context)
(define-cpointer-type _z3-symbol)
(define-cpointer-type _z3-ast)
(define-cpointer-type _z3-sort)
(define-cpointer-type _z3-app)
(define-cpointer-type _z3-pattern)
(define-cpointer-type _z3-model)
(define-cpointer-type _z3-func-decl)

(define _z3-error-handler (_fun #:keep #t _int -> _void))

(define-syntax defz3
  (syntax-rules (:)
    [(_ name : type ...)
     (begin
       (define name
         (get-ffi-obj (regexp-replaces 'name '((#rx"-" "_")
                                               (#rx"^" "Z3_")
                                               (#rx"!$" "")))
                      libz3 (_fun type ...)))
       (provide name))]))

(defz3 mk-config : -> _z3-config)
(defz3 set-param-value! : _z3-config _string _string -> _void)
(defz3 mk-context : _z3-config -> _z3-context)

(defz3 parse-smtlib2-string :
  _z3-context
  _string
  _uint
  (_or-null (_ptr i _z3-symbol))
  (_or-null (_ptr i _z3-sort))
  _uint
  (_or-null (_ptr i _z3-symbol))
  (_or-null (_ptr i _z3-func-decl)) -> _z3-ast)

;; -> string functions
(defz3 context-to-string : _z3-context -> _string)
(defz3 ast-to-string : _z3-ast -> _string)
