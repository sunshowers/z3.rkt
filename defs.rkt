#lang racket
;; Indicates an instance of a datatype (e.g. (List Int) for List).
(struct datatype-instance (z3-sort fns))
(provide datatype-instance
         datatype-instance-z3-sort
         datatype-instance-fns)