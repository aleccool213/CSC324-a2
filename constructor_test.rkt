#lang plai

(require "constructor.rkt")

(define (random_func r)
  (+ r 5)
)

(class-init Point (a b) (x y z)
  [
   (init a b)
   (let*
     (
       [r (random_func 5)]
       [x (random_func r)]
       [y (list b 100 r)]
       [z "you are cool"]
     )
     x
   )
  ]
)

(test (let ([p (Point 1 2)])
        (p "x"))
      11
)

(test (let ([p (Point 1 2)])
        (p "y"))
    (list 2 100 11)
)

(test (let ([p (Point 1 2)])
        (p "r"))
    "Unrecognized message!" 
)
