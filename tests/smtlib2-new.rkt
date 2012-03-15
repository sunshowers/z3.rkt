#lang racket

(require "../smtlib2-parser.rkt")

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

(with-context
 (new-context-info #t)
 (declare-datatypes () ((S A B C)))
 (declare-fun a () S)
 (declare-fun b () S)
 (declare-fun c () S)
 (assert (distinct a b c))
 (displayln (check-sat)))
