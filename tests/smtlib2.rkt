#lang racket

(require "../api.rkt")

(define ctx (make-model-context))

(define ast (parse-smtlib2 ctx '((declare-fun a () (_ BitVec 8))
                                 (assert (bvuge a \#x10))
                                 (assert (bvule a \#xf0)))))

(displayln (z3:ast-to-string ctx ast))
(z3:assert-cnstr ctx ast)
(displayln (z3:context-to-string ctx))
(let-values ([(rv model) (z3:check-and-get-model ctx)])
  (displayln rv)
  (displayln (z3:model-to-string ctx model)))
