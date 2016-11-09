#lang plai
(require "peek.rkt")

(-< 1 2 3 4)
(test (peek)
    (quote (-< 2 3 4))
)
(next)
(test (peek)
    (quote (-< 3 4))
)
