#lang racket/base

(require racket/contract/base)
(require (prefix-in z3: "z3-wrapper.rkt")
         "utils.rkt"
         "parser.rkt"
         "builtins.rkt")

(define (make-config #:model? [model? #t]
                     #:mbqi? [mbqi? #f]
                     #:macro-finder? [macro-finder? #t])
  (let ([config (z3:mk-config)])
    (z3:set-param-value! config "MODEL" (if model? "true" "false"))
    (z3:set-param-value! config "MBQI" (if mbqi? "true" "false"))
    (z3:set-param-value! config "MACRO_FINDER" (if macro-finder? "true" "false"))
    config))

(define (smt:new-context-info #:model? [model? #t]
                              #:logic [logic #f]
                              #:mbqi? [mbqi? #f]
                              #:macro-finder? [macro-finder? #t])
  (define ctx (z3:mk-context (make-config #:model? model? #:mbqi? mbqi? #:macro-finder? macro-finder?)))
  (when logic (z3:set-logic ctx logic))
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
 (contract-out
  [smt:new-context-info (->* () (#:model? boolean? #:logic string? #:mbqi? boolean? #:macro-finder? boolean?) z3ctx?)]))
