#lang scheme

(define (f x y)
  (define (g z)
    (if (<= (abs (- x z)) (abs (- y z)))
        x
        y))
  g)

(define posneg (f -1 1))

(display (posneg 21))
(display (posneg -3))
(display ((f 10 20) 10))