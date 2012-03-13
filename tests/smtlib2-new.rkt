#lang racket

(require "../api.rkt")
(require "../smtlib2-parser.rkt")

(with-context
 (new-context-info #t)
  (declare-fun a () Int)
  (assert (> a 10))
  (assert (< a 20))
  (check-sat))

(with-context
 (new-context-info #t)
  (declare-fun p () Bool)
  (assert (and p (not p)))
  (check-sat))

(with-context
 (new-context-info #t)
  (declare-fun a () Int)
  (assert (> a 10))
  (assert (< a 20))
  (displayln (check-sat))
  (assert (= (+ a 5) 16))
  (displayln (check-sat)))

(with-context
 (new-context-info #t)
 (declare-fun a () (List Int))
 (declare-fun b () (List Int))
 (assert (= a (cons 4 (cons 5 (nil)))))
 (assert (not (= b (nil))))
 (assert (< (head b) (- (head a) 2)))
 (assert (= (tail b) a))
 (displayln (check-sat))
 (displayln (z3-eval a))
 (displayln (z3-eval b)))
