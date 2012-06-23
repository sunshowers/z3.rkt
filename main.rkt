#lang racket/base

(require racket/contract/base)
(require (prefix-in z3: "z3-wrapper.rkt")
         "utils.rkt"
         "parser.rkt"
         "builtins.rkt"
         "derived.rkt")

(define z3-default-overrides #hasheq((#:macro-finder? . #t)))

(define (new-context-proc kws kw-args . rest)
  (unless (null? rest)
    (apply raise-arity-error 'smt:new-context 0 rest))
  (define config (z3:mk-config))
  (define params (hash-copy z3-default-overrides))
  (for ([kw (in-list kws)]
        [kw-arg (in-list kw-args)])
    (hash-set! params kw kw-arg))
  (for ([(kw kw-arg) (in-hash params)]
        #:unless (eq? kw '#:logic))
    (define-values (kw-str kw-arg-str) (z3:keyword-arg->_z3-param kw kw-arg))
    (z3:set-param-value! config kw-str kw-arg-str))

  (define ctx (z3:mk-context config))
  (define logic (hash-ref params '#:logic #f))
  (when logic (z3:set-logic ctx logic))
  (define vals (make-hash))
  (define sorts (make-hash))
  (define new-info (z3ctx ctx vals sorts (box #f)))
  (smt:with-context
   new-info
   (init-builtins))
  new-info)

; For a list of keyword arguments smt:new-context accepts, see
; http://research.microsoft.com/en-us/um/redmond/projects/z3/config.html.
; All keywords are in standard Racket form, with the words lowercased, the
; underscores changed to hyphens, and a ? suffixed to boolean arguments.
(define smt:new-context (make-keyword-procedure new-context-proc))

(provide
 (all-from-out "parser.rkt"
               "builtins.rkt"
               "derived.rkt")
 smt:new-context)
