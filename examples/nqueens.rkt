#lang racket

(require racket/generator)
(require "../smtlib2-parser.rkt")

(define (integer->queen n)
  (string->symbol (string-append "Q" (number->string n))))

(define (solve-nqueens n)
  (smt:with-context
   (smt:new-context-info)
   (define qs (smt:make-fun/list n () Int))
   (for ([q (in-list qs)])
     (smt:assert (and/s (>=/s q 0) (</s q n)))) ; columns are within bounds
   (smt:assert (apply distinct/s qs)) ; all columns distinct
   ;; no two queens on the same diagonal
   (for* ([(qi i) (in-indexed qs)]
          [(qj j) (in-indexed (stop-before qs ((curry eq?) qi)))])
     (smt:assert (and/s (distinct/s (-/s qi qj) (- i j)) (distinct/s (-/s qi qj) (- j i)))))
   (define seq
     (generator ()
       (let loop ()
         (yield (eq? (smt:check-sat) 'sat))
         ;; Get a new model by blocking out the current one
         (smt:assert (not/s (apply and/s
                                   (for/list ([q (in-list qs)]) (=/s q (smt:eval q))))))
         (loop))))
   (sequence-length (in-producer seq #f))))

(provide solve-nqueens)
