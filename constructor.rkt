#|
  class-init

  examples:

  (class-init Point (x y z)
    [
      (init (a b))
      (
        [r (random_func a)]
        [x (random_func r)]
        [y (random_func (list b 100 r))]
        [z "you are cool"]
      )
    ]
  )

|#






(define-syntax class-init
  (syntax-rules (init)
   [
     (class <Class> (<attr> ...)
        [(init <param> ...) <body>] ...)
   ]
  )
)
