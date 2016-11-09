#lang plai
(require "choice_uses.rkt")
(require "choice.rkt")

; Helper for comparing lists of subsets
(define (compare-multi lst1 lst2)
  (and (equal? (length lst1) (length lst2))
       (equal? (list->set (map (lambda (x) (sort x <)) lst1))
               (list->set (map (lambda (x) (sort x <)) lst2)))))


; Tests for subsets
;(test (all (subsets '()))
;      '(())
;)

;(test (compare-multi (all (subsets '(1)))
;                     '(() (1)))
;      #t)
;
;(test (compare-multi (all (subsets '(1 2 3)))
;                     '(()
;                       (1)
;                       (2)
;                       (2 1)
;                       (3)
;                       (1 3)
;                       (2 3)
;                       (1 2 3)))
;      #t)

; Test for sudoku-4
(define grid1
  '((1 2 3 4)
    ("" "" 1 "")
    ("" "" 2 3)
    (2 "" "" 1))
)

(define valid-soduku
  '((1 2 3 4)
    (3 4 1 2)
    (4 1 2 3)
    (2 3 4 1))
)

(define invalid-soduku-rows
  '((1 2 3 4)
    (3 4 1 2)
    (4 1 2 100)
    (2 3 4 1))
)

(define invalid-soduku-columns
  '((1 2 3 4)
    (3 4 1 2)
    (4 3 2 1)
    (2 3 4 1))
)

(define invalid-soduku-squares
  '((1 3 2 4)
    (3 4 1 2)
    (4 2 3 1)
    (2 1 4 3))
)

(test (checkSudoku valid-soduku)
  #t
)

(test (check-rows valid-soduku)
  #t
)

(test (check-rows invalid-soduku-rows)
  #f
)

(test (check-columns valid-soduku)
  #t
)

(test (check-columns invalid-soduku-columns)
  #f
)

(test (checkSudoku valid-soduku)
  #t
)

(test (check-squares valid-soduku)
  #t
)

(test (check-squares valid-soduku)
  #t
)

(test (check-squares invalid-soduku-squares)
  #f
)

(test (all (generate-solutions grid1))
  '(
    '((1 2 3 4)
      (3 4 1 2)
      (4 1 2 3)
      (2 3 4 1))
   )
)
;; Clear stack
;(clear)
;
;; Test for fold-<
;(test (fold-< max 0 (sin (* (-< 1 2 3 4) (+ (-< 100 200) (-< 1 2)))))
;      0.9948267913584064)
