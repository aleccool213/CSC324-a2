#|
  class-init

  A macro which is used to define a class in racket.
  This macro allows to specify the initialization method.
  The initialization method only sets values for the classes attributes.

  The macro is very similar to a origin class macro except now accepts a list of
  attributes which will be instance variables.

  Here is an example of the class init function from the python one in the handout
  using our new macro syntax.

  (define (random_func r)
    (+ r 5)
  )

  (class-init Point (a b) (x y z)
    [
      (init)
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
     (
       class <Class> (<init-params> ...) (<instance-var> ...)
        [(init) <body>]
     )
     (let*
       ; perform actions in the init function
       ([x 10])
       ; return resulting object
       (lambda (msg)
         (cond [(equal? msg (id->string <instance-var>)) x]
               ...
               [else "Unrecognized message!"]))
     )

   ]
  )
)
