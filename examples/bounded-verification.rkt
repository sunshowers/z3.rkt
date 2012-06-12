#lang racket

(require "../main.rkt"
         "list-helpers.rkt")
(require racket/function)

;; Calculate a relation (less than, greater than etc) for a list
(define-syntax-rule (define-z3-list-relation fn op)
  (define (fn n)
    (smt:define-fun fn-op ((x Int) (ys IntList)) IntList
                    (if (zero? n)
                        nil/s
                        (let ([subfn (fn (sub1 n))])
                          (ite/s (=/s ys nil/s)
                                 nil/s
                                 (ite/s (op (head/s ys) x)
                                        (insert/s (head/s ys) (subfn x (tail/s ys)))
                                        (subfn x (tail/s ys)))))))
    fn-op))

(define-z3-list-relation make-lt </s)
(define-z3-list-relation make-gt >/s)
(define-z3-list-relation make-le <=/s)
(define-z3-list-relation make-ge >=/s)

;; Define quicksort for a list. The lessop and greaterop functions are what
;; determine whether the quicksort is buggy. (lt/ge and le/gt are fine, lt/gt
;; and le/ge are broken).
(define (make-qsort n lessop-fn greaterop-fn)
  (smt:define-fun qsort ((xs IntList)) IntList
                  (if (zero? n)
                      nil/s
                      ; From here on is the usual definition of quicksort.
                      (ite/s (=/s xs nil/s)
                             nil/s
                             (let* ([subqsort (make-qsort (sub1 n) lessop-fn greaterop-fn)]
                                    [pivot (head/s xs)]
                                    [rest (tail/s xs)]
                                    [left-sorted (subqsort ((lessop-fn (sub1 n)) pivot rest))]
                                    [right-sorted (subqsort ((greaterop-fn (sub1 n)) pivot rest))])
                               ((make-append (sub1 n)) left-sorted (insert/s pivot right-sorted))))))
  qsort)

(define make-correct-qsort1 (curryr make-qsort make-le make-gt))
(define make-correct-qsort2 (curryr make-qsort make-lt make-ge))
(define make-buggy-qsort1 (curryr make-qsort make-lt make-gt))
(define make-buggy-qsort2 (curryr make-qsort make-le make-ge))

(provide make-correct-qsort1
         make-correct-qsort2
         make-buggy-qsort1
         make-buggy-qsort2)
