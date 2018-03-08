#lang scheme

(define (pascal n)
  (cond
    ((= n 1) '(1))
    (#t (let ((prev (pascal (- n 1))))
          (map + (cons 0 prev) (append prev '(0)))
          )
        )
    )
  )

(define (product list)
  (foldl * 1 list)
  )

(define (myReverse li)
  (foldl cons '() li))

(define (myMin li)
  (foldl (lambda (x y) (if (< x y) x y)) (car li) li)
  )

(define (dot v1 v2)
  (foldl + 0 (map * v1 v2))
  )

(define (matMul mat v1)
  (helpMatMul mat v1 '())
  )

(define (helpMatMul mat v1 matEnd)
  (cond
    ((null? mat) matEnd)
    (#t (helpMatMul (cdr mat) v1 (append matEnd (list(dot (car mat) v1)))))
    )
  )

(define (matMulBetter mat v1)
  (map (lambda (row) (dot row v1)) mat))

(define (transpose mat)
  (apply map list mat)
  )

(define (transpose2 mat)
  (cond
    ((null? (car mat)) '())
    (#t (cons (map car mat) (transpose2 (map cdr mat))))
    )
  )

(define (mxm m1 m2)
  (let ((trans (transpose m2)))
  (map (lambda (x) (matMulBetter trans x)) m1))
  )
  
