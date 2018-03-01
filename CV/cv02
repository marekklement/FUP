(define (revs xs)
  (cond
    ((null? xs) '())
    (#t (append (revs (cdr xs)) (list (car xs))))))


(define (revs2 xs acc)
  (cond
    ((null? xs) acc)
    (#t (revs2 (cdr xs) (cons (car xs) acc)))))

(define (flatten lst)
  (cond
    ((null? lst) lst)
    ((list? (car lst)) (append (flatten (cdr lst)) (flatten (car lst))))
    (#t (cons (car lst) (flatten (cdr lst))))
    )
  )

(define (dist a b)
  (let ((dx (- (car a) (car b)))
        (dy (- (cadr a) (cadr b))))
    (sqrt (+ (* dx dx) (* dy dy)))))

(define (filter fun? lst)
  (if (null? lst) lst
    (let ((ret (filter fun? (cdr lst))))
      (if (fun? (car lst)) (cons (car lst) ret) ret))))
        

(define (rem el lst)
  (cond
    ((null? lst) lst)
    (#t (filter (lambda (x) (not (= x el))) lst))))

(define (rem-dup lst)
  (cond
    ((null? lst) lst)
    (#t (rem (filter (lambda (x) (not (= x (car lst))) (cdr lst))))