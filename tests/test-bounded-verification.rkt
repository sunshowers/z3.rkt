#lang racket

(require "../main.rkt"
         "../examples/list-helpers.rkt"
         "../examples/bounded-verification.rkt")
(require rackunit)

(define (check-qsort-model make-qsort correct?)
  (smt:with-context
   (smt:new-context)
   (define len (make-length 5))
   ; Check lists of up to length 4
   (define qsort (make-qsort 4))
   (smt:declare-fun xs () IntList)
   (smt:assert (<=/s (len xs) 4))
   ; Attempt to prove that the length is the same. Proving is done
   ; by asserting the negation and checking for unsatisfiability.
   (smt:assert (not/s (=/s (len (qsort xs)) (len xs))))
   (check-eq? (smt:check-sat) (if correct? 'unsat 'sat))))

(define/provide-test-suite test-bounded-verification
  (test-case
   "Quick sort: correct (<=, >)"
   (check-qsort-model make-correct-qsort1 #t))
  (test-case
   "Quick sort: correct (<, >=)"
   (check-qsort-model make-correct-qsort2 #t))
  (test-case
   "Quick sort: broken (<, >)"
   (check-qsort-model make-buggy-qsort1 #f))
  (test-case
   "Quick sort: broken (<=, >=)"
   (check-qsort-model make-buggy-qsort2 #f)))
