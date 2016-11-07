#lang plai

(require "constructor.rkt")

(define (random_func r)
  (+ r 5)
)

(class-init Point (a b) (x y z)
  [
   (init a b)
   (
     [r (random_func 5)]
     [x (random_func r)]
     [y (list b 100 r)]
     [z "you are cool"]
   )
  ]
)

(test (let ([p (Point 1 2)])
        (p "x"))
      15
)

(test (let ([p (Point 1 2)])
        (p "y"))
    '(2 100 10)
)

(test (let ([p (Point 1 2)])
        (p "r"))
    "Unrecognized message!"
)
