#lang racket
#| Assignment 2 - Classes

This file contains your work for Questions 1 and 2, extending the basic
class macro to include support for traits and some basic introspection.
|#
(provide class-meta class-trait class)

; QUESTION 1 (metaprogramming).
(define-syntax class-meta
  (syntax-rules ()
    [(class <Class> (<attr> ...)
       [(<method> <param> ...) <body>] ...)
     (define (<Class> <attr> ...)
       (lambda (msg)
         (cond
              [(equal? msg (id->string <attr>)) <attr>]
               ...
               [
                (equal? msg (id->string <method>))
                (lambda (<param> ...) <body>)
               ]
               ...
               [
                (equal? msg "_attributes")
                (sort
                  (map
                    (lambda (curr-attr curr-value)
                      (list curr-attr curr-value)
                    )
                    (list
                      (id->string <attr>)
                      ...
                    )
                    (list
                      <attr>
                      ...
                    )
                  )
                  #:key car string<?
                )
               ]
               [
                (equal? msg "_methods")
                (sort
                  (map
                    (lambda (curr-method curr-value)
                      (list curr-method curr-value)
                    )
                    (list
                      (id->string <method>)
                      ...
                    )
                    (list
                      (lambda (<param> ...) <body>)
                      ...
                    )
                  )
                  #:key car string<?
                )
               ]
               [else "Unrecognized message!"]
          )
        )
       )
    ]
  )
)


; QUESTION 2 (traits).
(define-syntax class-trait
  (syntax-rules (with)
    [(class-trait <Class> (<attr> ...) (with)
        ((<method> <param> ...) <body>) ...)
      (define (<Class> <attr> ...)
        (lambda (msg)
            (cond [(equal? msg (id->string <attr>)) <attr>]
                  ...
                  [(equal? msg (id->string <method>))
                   (lambda (<param> ...) <body>)]
                  ...
                  [else "Unrecognized message!"]
            )
        )
      )
    ]
    [
      (class-trait <Class> (<attr> ...) (with <trait>)
        ((<method> <param> ...) <body>) ...
      )
      (define (<Class> <attr> ...)
        (<trait>
          (lambda (msg)
            (cond [(equal? msg (id->string <attr>)) <attr>]
                  ...
                  [(equal? msg (id->string <method>))
                   (lambda (<param> ...) <body>)]
                  ...
                  [else "Unrecognized message!"]
            )
          )
        )
      )
    ]
  )
)

; -----------------------------------------------------------------------------
; Class macro. This section is just for your reference.
; -----------------------------------------------------------------------------
(define-syntax class
  (syntax-rules ()
    [(class <Class> (<attr> ...)
       [(<method> <param> ...) <body>] ...)
     (define (<Class> <attr> ...)
       (lambda (msg)
         (cond [(equal? msg (id->string <attr>)) <attr>]
               ...
               [(equal? msg (id->string <method>))
                (lambda (<param> ...) <body>)]
               ...
               [else "Unrecognized message!"]))
     )
    ]
  )
)

(define-syntax id->string
  (syntax-rules ()
    [(id->string <id>)
     (symbol->string (quote <id>))]))
