#lang racket

(require "../smtlib2-parser.rkt")

(define (sudoku-grid-distinct args)
  (smt:assert (distinct ,@(map (λ (arg) `(select sudoku-grid ,arg)) args))))
(define (row-distinct n)
  (sudoku-grid-distinct (sequence->list (in-range n (+ n 9)))))
(define (column-distinct n)
  (sudoku-grid-distinct (sequence->list (in-range n (+ n 81) 9))))
(define (box-distinct n)
  (sudoku-grid-distinct (map ((curry +) n) '(0 1 2 9 10 11 18 19 20))))

(define (add-sudoku-grid-rules)
  (for ([n (in-range 0 81 9)])
    (row-distinct n))
  (for ([n (in-range 0 9)])
    (column-distinct n))
  (for ([n '(0 3 6 27 30 33 54 57 60)])
    (box-distinct n)))

(define (char->sudoku c)
  (string->symbol (list->string (list #\S c))))

(define (sudoku->char s)
  (string-ref (symbol->string s) 1))

(define (add-grid grid n)
  (if (empty? grid)
      (void)
      (begin
        (unless (eq? (first grid) #\_)
          (smt:assert (= (select sudoku-grid ,n) ,(char->sudoku (first grid)))))
        (add-grid (rest grid) (+ n 1)))))

;; Given a grid (81-element list where known entries are characters #\1-#\9 and
;; unknown entries are _), solve Sudoku for the grid and return #f if no
;; solutions are possible, and an 81-element list if a solution is possible.
(define (solve-sudoku grid)
  (smt:with-context
   (smt:new-context-info)
   (smt:declare-datatypes () ((Sudoku S1 S2 S3 S4 S5 S6 S7 S8 S9)))
   (smt:declare-fun sudoku-grid () (Array Int Sudoku))
   (add-sudoku-grid-rules) ; Plug in the grid rules (row, column, box)
   (add-grid grid 0)
   (define sat (smt:check-sat))
   (if (eq? sat 'sat)
       (map (λ (x) (sudoku->char (smt:eval (select sudoku-grid ,x))))
            (sequence->list (in-range 0 81))) ; Retrieve all the values
       #f)))

(define (solve-sudoku/string str)
  (let* ([solution-grid (solve-sudoku (sequence->list (in-string str)))])
    (if solution-grid (list->string solution-grid) #f)))

(provide solve-sudoku solve-sudoku/string)
