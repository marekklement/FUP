(define (discr a b c) (- (* b b) (* 4 a c)))
(define (minus a) (- 0 a))
(define (roots a b c)
  (cons (/ (+ (minus b) (sqrt (discr a b c))) (* 2 a)) (/ (- (minus b) (sqrt (discr a b c))) (* 2 a))))

(define (my-even? n)
  (cond
    ((= n 1) #f)
    ((= n 0) #t)
    ((< n 1) (my-even? (+ n 2)))
    (#t (my-even? (- n 2)))))

(define (fibonaci n)
  (cond
    ((= n 0) 0)
    ((= n 1) 1)
    ((< n 0) (quote "Wrong"))
    (else (+ (fibonaci (- n 1)) (fibonaci (- n 2))))))
(define (fib n k f0 f1)
  (cond
    ((= n 0) 0)
    ((< n 0) (quote "Wrong Number"))
    ((= n k) f1)
    (else (fib n (+ k 1) f1 (+ f0 f1)))))

(define (take-even l)
  (cond
    ((null? l) '())
    ((my-even? (car l)) (cons((car l) (take-even (cdr l)))))
    (else (take-even (cdr l)))))
   
