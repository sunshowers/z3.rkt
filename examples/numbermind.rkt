#lang racket/base

(require "../main.rkt")
(require racket/function
         racket/match)

;; A solution to Project Euler problem 185.
;; http://projecteuler.net/index.php?section=problems&id=185

(define (solve-numbermind guesses)
  (define varcount (string-length (car (car guesses))))
  (smt:with-context
   (smt:new-context)
   (define vars (smt:make-fun/list varcount () Int))
   ;; Every variable is between 0 and 9
   (for ([var vars]) (smt:assert (and/s (>=/s var 0) (<=/s var 9))))
   ;; Set up the constraints
   (for ([attempt (in-list guesses)])
     (match-let ([(list guess correct) attempt])
       (define correct-lhs (apply +/s
                                  (for/list ([x guess]
                                             [var vars])
                                    (ite/s (=/s var (- (char->integer x) 48)) 1 0))))
       (smt:assert (=/s correct-lhs correct))))
   (define sat (smt:check-sat))
   (if (eq? sat 'sat)
       (list->string (map (compose integer->char (curry + 48)) (map smt:eval vars)))
       #f)))

(provide solve-numbermind)
