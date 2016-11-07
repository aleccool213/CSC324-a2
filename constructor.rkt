#lang racket

#|
  class-init

  A macro which is used to define a Class in racket.
  This macro allows to specify the initialization method.
  The initialization method only sets values for the classes attributes.

  The macro is very similar to our original class macro except now accepts a list of
  attributes which will be instance variables.

  Here is an example of the class init function from the python one in the handout
  using our new macro syntax.

  (define (random_func r)
    (+ r 5)
  )

  (class-init Point (a b) (x y z)
    [
      (init a b)
      (
        [r (random_func a)]
        [x (random_func r)]
        [y (random_func (list b 100 r))]
        [z "you are cool"]
      )
    ]
  )
|#

(provide class-init)

(define-syntax class-init
  (syntax-rules (init)
    [(class <Class> (<attr> ...) (<instance-vars> ...)
       [(init) <init-body>] [(<method> <param> ...) <body>] ...)
     (define (<Class> <attr> ...)
       (lambda (msg)
         (let*
           <init-body>
           (cond [(equal? msg (id->string <attr>)) <attr>]
                 ...
                 [(equal? msg (id->string <instance-vars>)) <instance-vars>]
                 ...
                 [(equal? msg (id->string <method>))
                  (lambda (<param> ...) <body>)]
                 ...
                 [else "Unrecognized message!"]))
         )

     )
    ]
  )
)

(define-syntax id->string
  (syntax-rules ()
    [(id->string <id>)
     (symbol->string (quote <id>))]))
