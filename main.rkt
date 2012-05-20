#lang racket/base

(require (prefix-in z3: "z3-wrapper.rkt")
         "utils.rkt"
         "parser.rkt"
         "builtins.rkt")

(define (make-config #:model? [model? #t])
  (let ([config (z3:mk-config)])
    (z3:set-param-value! config "MODEL" (if model? "true" "false"))
    config))

(define (smt:new-context-info #:model? [model? #t])
  (define ctx (z3:mk-context (make-config #:model? model?)))
  (define vals (make-hash))
  (define sorts (make-hash))
  (define new-info (z3ctx ctx vals sorts (box #f)))
  (smt:with-context
   new-info
   (init-builtins))
  new-info)

(provide
 (all-from-out "parser.rkt"
               "builtins.rkt")
 smt:new-context-info)
