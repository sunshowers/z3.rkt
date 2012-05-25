#lang racket/base

(require "../main.rkt")
(require racket/match)

;; Returns a function that can reverse lists up to length n.
(define (make-reverse n)
  (define (make-reverse-internal n)
    (smt:define-fun reverse ((xs IntList) (accum IntList)) IntList
                    (if (zero? n)
                        accum
                        (let ([subreverse (make-reverse-internal (sub1 n))])
                          (ite/s (=/s xs nil/s)
                                 accum
                                 (subreverse (tail/s xs) (insert/s (head/s xs) accum))))))
    reverse)
  (define reverse (make-reverse-internal n))
  (λ (xs) (reverse xs nil/s)))

;; Returns a function that can perform appends up to n.
(define (make-append n)
  (define (make-append-internal n)
    (smt:define-fun append ((xs IntList) (ys IntList)) IntList
                    (if (zero? n)
                        ys
                        (let ([subappend (make-append-internal (sub1 n))])
                          (ite/s (=/s xs nil/s)
                                            ys
                                            (subappend (tail/s xs) (insert/s (head/s xs) ys))))))
    append)
  (define append (make-append-internal n))
  (define reverse (make-reverse n))
  (λ (xs ys) (append (reverse xs) ys)))

;; Calculates length of a list, assuming the maximum possible length is n.
(define (make-length n)
  (smt:define-fun len ((xs IntList)) Int
                  (if (zero? n)
                      0
                      (ite/s (=/s xs nil/s)
                             0
                             (let ([sublen (make-length (sub1 n))])
                               (+/s 1 (sublen (tail/s xs)))))))
  len)

(define (list->z3-list l)
  (if (eq? '() l)
      nil/s
      (insert/s (car l) (list->z3-list (cdr l)))))

;; XXX This doesn't actually work for actual Z3 lists, only for evaluated ones.
;; We use a hack right now to read in insert/s as "cons" and so on in parser.rkt.
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
