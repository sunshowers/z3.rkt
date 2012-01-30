#lang racket

(require "../api.rkt")
(require "../smtlib2-parser.rkt")

(with-context (new-context-info #t)
  (declare-sort MySort 0)
  (declare-fun Foo (Bool) (_ BitVec 8))
  MySort)
