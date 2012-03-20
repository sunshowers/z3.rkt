#lang racket

(require racket/generator)
(require "../smtlib2-parser.rkt")

(define (integer->queen n)
  (string->symbol (string-append "Q" (number->string n))))

(define (queens-distinct args)
  (smt:assert (distinct ,@(map (λ (arg) `(select queens ,arg)) args))))

(define (solve-nqueens n)
  (define queen-syms (sequence->list (sequence-map integer->queen (in-range 0 n))))
  (smt:with-context
   (smt:new-context-info)
   (smt:declare-fun queens () (Array Int Int))
   (for ([i (in-range 0 n)])
     (smt:assert (and (>= (select queens ,i) 0) (< (select queens ,i) ,n)))) ; columns are within bounds
   (queens-distinct (sequence->list (in-range 0 n))) ; all columns distinct
   ;; no two queens on the same diagonal
   (for* ([i (in-range 0 n)]
          [j (in-range 0 i)])
     (smt:assert (and (distinct (- (select queens ,i) (select queens ,j) ,(- i j)))
                      (distinct (- (select queens ,i) (select queens ,j) ,(- j i))))))
   (define seq
     (generator ()
       (let loop ()
         (yield (eq? (smt:check-sat) 'sat))
         ;; Get a new model by blocking out the current one
         (smt:assert (distinct queens (eval queens)))
         (loop))))
   (sequence-length (in-producer seq #f))))

(provide solve-nqueens)
