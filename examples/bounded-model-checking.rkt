#lang racket

(require "../main.rkt"
         "list-helpers.rkt")

;; Calculate a relation (less than, greater than etc) for a list
(define-syntax-rule (define-z3-list-relation fn op)
  (define (fn n)
    (smt:declare-fun fn-op (Int IntList) IntList)
    (if (zero? n)
        (smt:assert (forall/s ((x Int) (xs IntList))
                              (=/s (fn-op x xs) (nil/s))))
        (let ([subfn (fn (sub1 n))])
          (smt:assert (forall/s ((x Int) (xs IntList))
                                (=/s (fn-op x xs)
                                     (ite/s (=/s xs (nil/s))
                                            (nil/s)
                                            (ite/s (op (head/s xs) x)
                                                   (cons/s (head/s xs) (subfn x (tail/s xs)))
                                                   (subfn x (tail/s xs)))))))))
    fn-op))

(define-z3-list-relation make-lt </s)
(define-z3-list-relation make-gt >/s)
(define-z3-list-relation make-le <=/s)
(define-z3-list-relation make-ge >=/s)

;; Define quicksort for a list
(define (make-qsort n)
  (smt:declare-fun qsort (IntList) IntList)
  (if (zero? n)
      (smt:assert (forall/s ((xs IntList))
                            (=/s (qsort xs) (nil/s))))
      (smt:assert (forall/s ((xs IntList))
                            (=/s (qsort xs)
                                 (ite/s (=/s xs (nil/s))
                                        (nil/s)
                                        (let* ([subqsort (make-qsort (sub1 n))]
                                               [pivot (head/s xs)]
                                               [rest (tail/s xs)]
                                               [left-sorted (subqsort ((make-lt (sub1 n)) pivot rest))]
                                               [right-sorted (subqsort ((make-gt (sub1 n)) pivot rest))])
                                          ((make-append (sub1 n)) left-sorted (cons/s pivot right-sorted))))))))
  qsort)

(define (check-qsort-model)
  (smt:with-context
   (smt:new-context-info)
   (define len (make-length 8))
   (define qsort (make-qsort 6))
   (smt:declare-fun unsorted () IntList)
   (smt:assert (=/s (len unsorted) 6))
   (smt:assert (not/s (=/s (len (qsort unsorted)) (len unsorted))))
   (displayln (smt:check-sat))
   (displayln (smt:eval unsorted))))

(check-qsort-model)
