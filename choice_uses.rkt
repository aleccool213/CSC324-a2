#| Assignment 2 - Using Backtracking

This file contains starter code for questions 4-6,
which involve using backtracking in interesting ways, and
extending the functionality of the backtracking library.
|#
#lang racket

; Import choice API
(require "choice.rkt")

; Export functions for testing. Please don't change this line!
(provide subsets sudoku-4 fold-< distrib extend check-squares checkSudoku check-rows check-columns generate-solutions)

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
(define (sudoku-4 grid)
  (?- checkSudoku (generate-solutions grid))
)

#|
  generate-solutions

  grid: an incomplete sodoku grid

  - Returns a -< (yield) which generate all possible permuations
  of the grid. A permuation of the grid is all of the quotes filled with
  integers.
|#
(define (generate-solutions grid)
  ; count empty slots, ()(empty_slots) * 4!) are how many possible solutions there are
  ; something like this
  (map
    (lambda (row)
      (map
        (lambda (point)
          (if (equal? point "")
            (-< 1 2 3 4)
            point
          )
        )
        row
      )
    )
    grid
  )
)



(define (permutation lst)
  (if (empty? lst)
      '()
      (insert (permutation (rest lst))
              (first lst))))

(define (insert lst val)
  (if (empty? lst)
      (list val)
      (-< (cons val lst)
          (cons (first lst)
                (insert (rest lst) val)))))


#|
  checkSudoku

  grid: a complete soduku grid

  - Returns true if the soduku grid is valid :D
|#
(define (checkSudoku grid)
  (and
    (check-rows grid)
    (check-columns grid)
    (check-squares grid)
  )
)

#|
  check-rows

  grid: a complete soduku grid

  - Returns true if all of the rows contain the set (1 2 3 4)
|#
(define (check-rows grid)
  (let
    ([x (map (lambda (curr)
              (if ;append if the list contains all four ints
                (equal? (list->set '(1 2 3 4)) (list->set curr))
                curr
                '()
              ))
          grid
        )
    ])
    ; if we have the same list as our init grid, we know its valid
    (if (equal? x grid) #t #f)
  )
)


#|
  check-columns

  grid: a complete soduku grid

  - Returns true if all of the columns contain the set (1 2 3 4)
|#
(define (check-columns grid)
  (check-rows (map
    (lambda (curr)
      (list
        (list-ref (first grid) curr)
        (list-ref (second grid) curr)
        (list-ref (third grid) curr)
        (list-ref (fourth grid) curr)
      )
    )
    '(0 1 2 3)
  ))
)

#|
  check-squares

  grid: a complete soduku grid

  - Returns true if all of the squares contain the set (1 2 3 4)
|#
(define (check-squares grid)
  (let
    ([squares-to-rows (foldl
                        (lambda (curr result)
                          ; append two lists to the result
                          ; first square and second square
                          (append
                            (list
                              (list
                                (list-ref (list-ref grid curr) 0)
                                (list-ref (list-ref grid curr) 1)
                                (list-ref (list-ref grid (+ curr 1)) 0)
                                (list-ref (list-ref grid (+ curr 1)) 1)
                              )
                            )
                            (list
                              (list
                                (list-ref (list-ref grid curr) 2)
                                (list-ref (list-ref grid curr) 3)
                                (list-ref (list-ref grid (+ curr 1)) 2)
                                (list-ref (list-ref grid (+ curr 1)) 3)
                              )
                            )
                            result
                          )
                        )
                        '()
                        '(0 2)
                      )
     ])
    (check-rows squares-to-rows)
  )
)

; QUESTION 6
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
    [[fold-< <combine> <init> <expr>]
      [let* ([fold-results <init>]
             [fold-helper
              (lambda (e)
                (set! fold-results (<combine> e fold-results ))
                (next)
                fold-results)])
       (fold-helper <expr>)]]
    ))
