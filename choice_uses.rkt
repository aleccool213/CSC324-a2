#| Assignment 2 - Using Backtracking

This file contains starter code for questions 4-6,
which involve using backtracking in interesting ways, and
extending the functionality of the backtracking library.
|#
#lang racket

; Import choice API
(require "choice.rkt")

; Export functions for testing. Please don't change this line!
(provide subsets sudoku-4 fold-< distrib extend)

; QUESTION 3
#|
(subsets lst)
  lst: a list

  A choice expression which yields a subset of 'lst'.
  Repeated calls to 'next' should yield *all* other subsets
  (i.e., 'next' returns "false." only when all subsets of 'lst'
  have been returned).

  The subsets can be yielded in any order; however, no subset
  can appear twice.

  Note that:
    - A subset isn't the same as a sublist. Items don't have to be consecutive.
    - A subset can be empty; the empty set is a subset of any list.
    - Order doesn't matter in subsets

  The following is an example of how 'subsets' is used.
  Note that your implementation might yield the subsets
  in a different order than the one shown here.

> (subsets '(1 2))
'()
> (next)
'(1)
> (next)
'(2)
> (next)
'(1 2)
> (next)
"false."
|#

(define (subsets lst)
  (if (empty? lst)
      (list '())
      (extend (subsets (rest lst))
              (first lst))))

(define (extend lst val)
  (append lst (list (distrib lst val)))
)

(define (distrib lst val)
  (if (empty? lst)
      '()
      (cons (cons val (first lst))
            (distrib (rest lst) val))))

; QUESTION 4
#|
(sudoku-4 puzzle)
  puzzle: a nested list representing a 4-by-4 Sudoku puzzle

  A choice expression that represents possible solutions to the puzzle.
  Upon evaluation, just one solution is returned, but repeated calls
  to 'next' produces more possible solutions, if any.

  Hint: use the ?- function in your solution. Again, your main task
  is just to correctly express the constraints, and let the computer
  do the work.
|#
(define sudoku-4 (void))

(define (checkSudoku list)
  (and (checkSquares list 4)(and (checkColumns list) (checkRows list))))

(define (checkRows list)
  (if (null? (first list))
      #t
      (if (has-duplicates? (first list))
          #f
          (checkRows (rest list)))
      ))
(define (checkColumns list)
  (if (null? (first list))
      #t
      (if (has-duplicates? (map (lambda (x) (first x)) list))
          #f
          (checkRows (map (lambda (x) (rest x)) list)))
  ))
(define (checkSquares list n)
  (if (or
       (or
        (or
         (has-duplicates? '((list-ref(list-ref list 0) 0) (list-ref(list-ref list 0) 1) (list-ref(list-ref list 1) 0) (list-ref(list-ref list 1) 1)))
         (has-duplicates? '((list-ref(list-ref list 0) 2) (list-ref(list-ref list 0) 3) (list-ref(list-ref list 1) 2) (list-ref(list-ref list 1) 3))))
        (has-duplicates? '((list-ref(list-ref list 2) 0) (list-ref(list-ref list 2) 1) (list-ref(list-ref list 3) 0) (list-ref(list-ref list 3) 1))))
       (has-duplicates? '((list-ref(list-ref list 2) 2) (list-ref(list-ref list 2) 3) (list-ref(list-ref list 3) 2) (list-ref(list-ref list 3) 3))))
      #f
      #t))

;change this code
(define (has-duplicates? lst)
  (cond
     [(empty? lst) #f]
     [(not (not (member (first lst) (rest lst)))) #t]
     [else (has-duplicates? (rest lst)) ]))

; QUESTION 5
#|
(fold-< combine init expr)
  combine: a binary function
  init: an initial value
  expr: a choice expression

  Evaluate all choices in <expr> and combine them, one at a time, with the
  initial value, and return the result.

  Note that the order of <combine>'s parameters is the same as foldl:
    1) The value of the next choice
    2) The value of <init>
|#
(define-syntax fold-<
  (syntax-rules ()
    ))
