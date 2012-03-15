#lang racket

(require rackunit)
(require "../smtlib2-parser.rkt")

(define test-booleans
  (test-suite
   "Test basic boolean formulas"

   (test-case
    "Test one constraint"
    (smt:with-context
     (smt:new-context-info)
     (smt:declare-fun p () Bool)
     (check-equal? (smt:check-sat) 'sat)
     (smt:assert (= p true))
     (check-equal? (smt:check-sat) 'sat)
     (check-equal? (smt:eval p) 'true)))

   (test-case
    "Test a basic contradiction"
    (smt:with-context
     (smt:new-context-info)
     (smt:declare-fun p () Bool)
     (smt:assert (and p (not p)))
     (check-equal? (smt:check-sat) 'unsat)))
   
   (test-case
    "Test implies"
    (smt:with-context
     (smt:new-context-info)
     (smt:declare-fun p () Bool)
     (smt:declare-fun q () Bool)
     (smt:assert (implies p q))
     (check-equal? (smt:check-sat) 'sat)
     (smt:assert (= p true))
     (check-equal? (smt:check-sat) 'sat)
     (check-equal? (smt:eval q) 'true)
     (smt:assert (= q false))
     (check-equal? (smt:check-sat) 'unsat)))))

(provide test-booleans)
