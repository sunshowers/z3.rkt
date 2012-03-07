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
 (assert (= a (cons 4 (cons 5 (nil)))))
 (displayln (check-sat))
 (displayln (z3-eval a)))
