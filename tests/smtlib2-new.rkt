#lang racket

(require "../smtlib2-parser.rkt")

(with-context
 (new-context-info #t)
 (declare-datatypes () ((S A B C)))
 (declare-fun a () S)
 (declare-fun b () S)
 (declare-fun c () S)
 (assert (distinct a b c))
 (displayln (check-sat)))
