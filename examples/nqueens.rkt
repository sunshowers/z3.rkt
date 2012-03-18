#lang racket

(require racket/generator)
(require "../smtlib2-parser.rkt")

(define (integer->queen n)
  (string->symbol (string-append "Q" (number->string n))))

(define (solve-nqueens n)
  (define queen-syms (sequence->list (sequence-map integer->queen (in-range 0 n))))
  (smt:with-context
   (smt:new-context-info)
   (for ([qi queen-syms])
     (smt:declare-fun ,qi () Int)
     (smt:assert (and (>= ,qi 0) (< ,qi ,n)))) ; columns are within bounds
   (smt:assert (distinct ,@queen-syms)) ; all columns distinct
   ;; no two queens on the same diagonal
   (for* ([(qi i) (in-indexed queen-syms)]
          [(qj j) (in-indexed (stop-before queen-syms (λ (x) (eq? x qi))))])
     (smt:assert (and (distinct (- ,qi ,qj) ,(- i j)) (distinct (- ,qi ,qj) ,(- j i)))))
   (define seq
     (generator ()
       (let loop ()
         (yield (smt:check-sat))
         ;; Get a new model by blocking out the current one
         (smt:assert (not (and ,@(map (λ (qi) `(= ,qi ,(smt:eval ,qi))) queen-syms))))
         (loop))))
   (sequence-length (in-producer seq 'unsat))))

(provide solve-nqueens)
