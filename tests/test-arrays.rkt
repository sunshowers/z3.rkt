#lang racket

(require rackunit)
(require "../main.rkt")

(define/provide-test-suite test-arrays
  (test-case
   "Test basic array operations"
   (smt:with-context
    (smt:new-context)
    (smt:declare-fun a () (Array Int Int))
    (smt:assert (=/s (select/s a 0) 5))
    (smt:assert (</s (select/s a 1) (select/s a 0)))
    (check-eq? (smt:check-sat) 'sat)
    (check-eq? (smt:eval (select/s a 0)) 5)
    (check-true (< (smt:eval (select/s a 1)) (smt:eval (select/s a 0))))))
  
  (test-case
   "Test array property assertions"
   ;; Example from Z3 guide at http://rise4fun.com/z3/tutorial/guide
   (smt:with-context
    (smt:new-context #:mbqi? #t)
    ;; A0, A1, A2, A3, A4 are "arrays" from Integers to Integers.
    (smt:declare-fun A0 (Int) Int)
    (smt:declare-fun A1 (Int) Int)
    (smt:declare-fun A2 (Int) Int)
    (smt:declare-fun A3 (Int) Int)
    (smt:declare-fun A4 (Int) Int)
    (smt:declare-fun n () Int)
    (smt:declare-fun l () Int)
    (smt:declare-fun k () Int)
    (smt:declare-fun x () Int)
    (smt:declare-fun y () Int)
    (smt:declare-fun w () Int)
    (smt:declare-fun z () Int)

    ;; A1 = A0[k <- w]
    (smt:assert (=/s (A1 k) w))
    (smt:assert (forall/s ((x Int)) (or/s (=/s x k) (=/s (A1 x) (A0 x)))))

    ;; A2 = A1[l <- x] = A0[k <- w][l <- x]
    (smt:assert (=/s (A2 l) x))
    (smt:assert (forall/s ((x Int)) (or/s (=/s x l) (=/s (A2 x) (A1 x)))))

    ;; A3 = A0[k <- y]
    (smt:assert (=/s (A3 k) y))
    (smt:assert (forall/s ((x Int)) (or/s (=/s x k) (=/s (A3 x) (A0 x)))))

    ;; A4 = A3[l <- z] = A0[k <- y][l <- z] 
    (smt:assert (=/s (A3 l) z))
    (smt:assert (forall/s ((x Int)) (or/s (=/s x l) (=/s (A4 x) (A3 x)))))

    (smt:assert (and/s (</s w x) (</s x y) (</s y z)))
    (smt:assert (and/s (</s 0 k) (</s k l) (</s l n)))
    (smt:assert (>/s (-/s l k) 1))

    ;; A2 is sorted in the interval [0,n-1]
    (smt:assert (forall/s ((i Int) (j Int))
                          (=>/s (and/s (<=/s 0 i) (<=/s i j) (<=/s j (-/s n 1)))
                                (<=/s (A2 i) (A2 j)))))
    (check-eq? (smt:check-sat) 'sat)

    ;; A4 is sorted in the interval [0,n-1]
    (smt:assert (forall/s ((i Int) (j Int))
                          (=>/s (and/s (<=/s 0 i) (<=/s i j) (<=/s j (-/s n 1)))
                                (<=/s (A4 i) (A4 j)))))
    (check-eq? (smt:check-sat) 'unsat)))
  
  (test-case
   "Test shifts on streams"
   ;; Another example from the Z3 guide
   ;; f and g are "streams"
   (smt:with-context
    (smt:new-context #:mbqi? #t)
    (smt:declare-fun f (Int) Int)
    (smt:declare-fun g (Int) Int)

    ;; the segment [a, n + a] of stream f is equal
    ;; to the segment [0, n] of stream g.
    (smt:declare-fun n () Int)
    (smt:declare-fun a () Int)
    (smt:assert (forall/s ((x Int)) (=>/s (and/s (<=/s 0 x) (<=/s x n))
                                          (=/s (f (+/s x a)) (g x)))))

    ;; adding some constraints to a
    (smt:assert (>/s a 10))
    (smt:assert (>=/s (f a) 2))
    (smt:assert (<=/s (g 3) (-/s 10)))

    (check-eq? (smt:check-sat) 'sat))))
