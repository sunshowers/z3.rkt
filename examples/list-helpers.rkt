#lang racket/base

(require "../main.rkt")
(require racket/match)

;; Returns a list of functions that can perform reverses. The head of the list can perform
;; reverse for a list of up to n, and so on.
(define (make-reverse n)
  (smt:declare-fun reverse (IntList IntList) IntList)
  (if (zero? n)
      (begin
        (smt:assert (forall/s ((xs IntList) (accum IntList))
                              (=/s (reverse xs accum) accum)))
        (list reverse))
      (let ([subreverse (make-reverse (sub1 n))])
        (smt:assert (forall/s ((xs IntList) (accum IntList))
                              (=/s (reverse xs accum) (ite/s (=/s xs (nil/s))
                                                             accum
                                                             ((car subreverse) (tail/s xs) (cons/s (head/s xs) accum))))))
        (cons reverse subreverse))))

;; Returns a function that can perform appends up to n.
(define (make-append n)
  (define (make-append-internal n)
    (smt:declare-fun append (IntList IntList) IntList)
    (if (zero? n)
        (begin
          (smt:assert (forall/s ((xs IntList) (ys IntList))
                                (=/s (append xs ys) ys)))
          append)
        (let ([subappend (make-append-internal (sub1 n))])
          (smt:assert (forall/s ((xs IntList) (ys IntList))
                                (=/s (append xs ys)
                                     (ite/s (=/s xs (nil/s))
                                            ys
                                            (subappend (tail/s xs) (cons/s (head/s xs) ys))))))
          append)))
  (define append-fn (make-append-internal n))
  (define reverse-fn (car (make-reverse n)))
  (λ (xs ys) (append-fn (reverse-fn xs (nil/s)) ys)))

;; Calculates length of a list, assuming the maximum possible length is n.
(define (make-length n)
  (smt:declare-fun len (IntList) Int)
  (if (zero? n)
      (smt:assert (forall/s ((xs IntList))
                            (=/s (len xs) 0)))
      (smt:assert (forall/s ((xs IntList))
                            (=/s (len xs) (ite/s (=/s xs (nil/s))
                                                 0
                                                 (let ([sublen (make-length (sub1 n))])
                                                   (+/s 1 (sublen (tail/s xs)))))))))
  len)

(define (list->z3-list l)
  (if (eq? '() l)
      (nil/s)
      (cons/s (car l) (list->z3-list (cdr l)))))

;; XXX This doesn't actually work for actual Z3 lists, only for evaluated ones.
;; We use a hack right now to read in cons/s as "cons" and so on in parser.rkt.
;; Improve this.
(define (z3-list->list zl)
  (match zl
    ['nil '()]
    [(list 'cons x zs) (cons x (z3-list->list zs))]))

(provide make-reverse
         make-append
         make-length
         list->z3-list
         z3-list->list)
