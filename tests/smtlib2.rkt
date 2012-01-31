#lang racket

(require rackunit "../api.rkt")

(define ctx (make-model-context))

(define ast (parse-smtlib2 ctx '((declare-fun p () Bool)
                                 (assert (and p (not p))))))

(displayln (z3:ast-to-string ctx ast))
(define smtlib2-tests
  (test-suite
   "SMTLIB2 tests"
   (test-case
    "Initial test works"
    (z3:assert-cnstr ctx ast)
    (displayln (z3:context-to-string ctx))
    (let-values ([(rv model) (z3:check-and-get-model ctx)])
      (check-equal? rv 'true)
      (let ([kind (z3:get-ast-kind ctx ast)])
        (check-equal? kind 'app)
        (displayln (z3:get-app-num-args ctx (z3:to-app ctx ast)))
        )))))

(provide smtlib2-tests)
