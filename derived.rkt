#lang racket/base

(require "parser.rkt"
         "builtins.rkt")

;; Functions that are written in terms of the base functions in main.rkt and
;; builtins.rkt.

;; Define a function using universal quantifiers as a sort of macro.
;; Note that defining recursive functions is possible but highly
;; recommended against.
(define-syntax define-fun
  (syntax-rules ()
    [(_ id () type body)
     (begin
       (smt:declare-fun id () type)
       (smt:assert (=/s id body)))]
    [(_ id ((argname argtype) ...) type body)
     (begin
       (smt:declare-fun id (argtype ...) type)
       (smt:assert (forall/s ((argname argtype) ...)
                             (=/s (id argname ...) body))))]))

(provide
 (prefix-out
  smt:
  (combine-out define-fun)))
