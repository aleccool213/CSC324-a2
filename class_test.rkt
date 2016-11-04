#lang plai

(require "class.rkt")


; Tests for Question 1

(class-meta Point-meta (x y)
  [
   (distance other-point)
   (let
     (
       [dx (- x (other-point "x"))]
       [dy (- y (other-point "y"))]
     )
     (sqrt (+ (* dx dx) (* dy dy))))
  ]
)

(test (let ([p (Point-meta 2 3)])
        (p "_attributes"))
      '(("x" 2) ("y" 3))
)

(class-meta Point-meta-none ())

(test (let ([p (Point-meta-none)])
        (p "_attributes"))
      '()
)

(test (let ([p (Point-meta 2 3)])
        (first (first (p "_methods"))))
      "distance")

(test (let* ([p (Point-meta 2 3)]
             [f (second (first (p "_methods")))])
        (f (Point-meta 10 20)))
      18.788294228055936)
;
;; Check alphabetical order
(class-meta A-meta (red blue green)
  [(white x)
   (+ x 10)]
  [(black x)
   (* x 2)])

(test (let ([p (A-meta "b" "g" "r")])
        (p "_attributes"))
      '(("blue" "g")
        ("green" "r")
        ("red" "b")))

(test (let ([p (A-meta 1 2 3)])
        (map first (p "_methods")))
      '("black" "white"))


; Traits for testing
(define (distance-trait obj)
  (lambda (msg)
    (cond [(equal? msg "distance-to-self")
           (lambda () ((obj "distance") obj))]
          [(equal? msg "closer")
           (lambda (obj1 obj2)
             (if (<= ((obj "distance") obj1)
                     ((obj "distance") obj2))
                 obj1
                 obj2))]
          [else (obj msg)])))

(define (distance-trait-2 obj)
  (lambda (msg)
    (cond [(equal? msg "distance-plus")
           (lambda (other x) (+ ((obj "distance") other) x))]
          [else (obj msg)])))

; Tests for Question 2

; zero traits


; One trait
(class-trait Point (x y) (with distance-trait)
  [(distance other-point)
   (let ([dx (- x (other-point "x"))]
         [dy (- y (other-point "y"))])
     (sqrt (+ (* dx dx) (* dy dy))))])

; two traits
(class-trait Point2 (x y) (with distance-trait distance-trait-2)
  [(distance other-point)
   (let ([dx (- x (other-point "x"))]
         [dy (- y (other-point "y"))])
     (sqrt (+ (* dx dx) (* dy dy))))])

; testing one trait class works with base methods
(test (let* ([p1 (Point 2 3)])
        (list (p1 "x") (p1 "y")))
      '(2 3))

; testing one trait class works with base methods
(test (let* ([p1 (Point 30 40)]
             [p2 (Point 15 40)])
        ((p1 "distance") p2))
      15)

; testing one trait class works with trait method
(test (let* ([p1 (Point 2 3)])
        ((p1 "distance-to-self")))
      0)

; testing one trait class works with trait method
(test (let* ([p1 (Point 30 40)]
             [p2 (Point 15 40)]
             [p3 (Point 31 39)]
             [p4 ((p1 "closer") p2 p3)])
        (list (p4 "x") (p4 "y")))
      '(31 39))

; testing two trait class works with trait method of second inherited trait
(test (let* ([p1 (Point2 30 40)]
             [p2 (Point 15 40)])
        ((p1 "distance-plus") p2 20))
      35)

;
(test (let* ([p1 (Point2 30 40)]
             [p2 (Point 15 40)])
        ((p1 "distance-plus") p2 20))
      35)
