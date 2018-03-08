(define get-maze
'(
(w w w w w w)
(w 2 w 0 w w)
(w 0 w 0 0 w)
(w 0 0 1 w w)
(w w w w w w)
)
)
(define right-hand-rule-prg
  '(
    (procedure start
      ( turn-right
        (if wall?
           ( turn-left
             (if wall?
                 (turn-left
                     (if wall?
                        turn-left
                        step
                     )
                 )
                 step
              )
           )
           step  
        )
        put-mark
        start
      )
    )   
    (procedure turn-right (turn-left turn-left turn-left))
  )
)

(define state1 (list get-maze (list 1 1) 'south))
(define state2 (list get-maze (list 2 4) 'north))
(define state3 (list get-maze (list 3 3) 'north))

;************************************************************
;************************************************************
;**********************Program*******************************
;************************************************************
;************************************************************

;////////////////////////////////////////////////////////////
;---------Finds position x/y in maze------------------------
;////////////////////////////////////////////////////////////
;---------Gets current position in maze
(define (get-current-maze-list row rowc maze)
  (cond
    ((= rowc row) (car maze))
    (#t (get-current-maze-list row (+ rowc 1) (cdr maze)))))

;---------Finds it by given x/y (shorten function)
(define (find-position x y maze)
  (get-current-maze-list y 0 (get-current-maze-list x 0 maze)))

;//////////////////////////////////////////////////////////
;<<<<<<<<<Functions for locating what I am facing>>>>>>>>>>
;//////////////////////////////////////////////////////////

;---------Define arguments sites
(define north 'north)
(define west 'west)
(define south 'south)
(define east 'east)

;--------Is it north?
(define (norh? where)
  (cond
    ((eqv? where north) #t)
    (#t #f)))

;--------Is it south?
(define (south? where)
  (cond
    ((eqv? where south) #t)
    (#t #f)))

;--------Is it west?
(define (west? where)
  (cond
    ((eqv? where west) #t)
    (#t #f)))

;--------Is it east?
(define (east? where)
  (cond
    ((eqv? where east) #t)
    (#t #f)))
;//////////////////////////////////////////////////////////
;<<<<<<<<<<<<<<<<<<<Wall?>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;//////////////////////////////////////////////////////////
(define (letter-for-wall state)
  (define maze (car state))
  (define x (caadr state))
  (define y (cadadr state))
  (define facing (caddr state))
  (cond
    ((norh? facing) (find-position (- x 1) y maze))
    ((south? facing) (find-position (+ x 1) y maze))
    ((west? facing) (find-position x (+ y 1) maze))
    ((east? facing) (find-position x (- y 1) maze))
    (#t 'w)
  ))

;(list get-maze (list 1 1) 'south)
(define (wall? state)
  (define letter (letter-for-wall state))
  (cond
    ((eqv? letter 'w) #t)
    (#t #f)
    ))

;//////////////////////////////////////////////////////////
;<<<<<<<<<<<<<<<<<<<North?>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;//////////////////////////////////////////////////////////
(define (north? state)
  (define facing (caddr state))
  (cond
    ((norh? facing) #t)
    (#t #f)
    )
  )

;//////////////////////////////////////////////////////////
;<<<<<<<<<<<<<<<<<<<Mark?>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;//////////////////////////////////////////////////////////
(define (mark? state)
  (let* ((x (caadr state))
         (y (cadadr state))
         (maze (car state))
         (letter (find-position x y maze)))
    (cond
      ((= letter 0) #f)
      (#t #t)
      )
    )
  )

;************************************************************
;************************************************************
;**********************State Modifiers***********************
;************************************************************
;************************************************************
(define (step state)
  (define maze (car state))
  (define x (caadr state))
  (define y (cadadr state))
  (define facing (caddr state))
  (cond
    ;check for wall
    ((wall? state) state)
    ((norh? facing) (list maze (list (- x 1) y) facing))
    ((south? facing) (list maze (list (+ x 1) y) facing))
    ((west? facing) (list maze (list x (+ y 1)) facing))
    ((east? facing) (list maze (list x (- y 1)) facing))
    )
  )

;//////////////////////////////////////////////////////////
;<<<<<<<<<<<<<<<<<<<Turn left>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;//////////////////////////////////////////////////////////
(define (turn-left state)
  (define maze (car state))
  (define x (caadr state))
  (define y (cadadr state))
  (define facing (caddr state))
  (cond
    ((norh? facing) (list maze (list x y) west))
    ((south? facing) (list maze (list x y) east))
    ((west? facing) (list maze (list x y) south))
    ((east? facing) (list maze (list x y) north))
    )
  )
  
;//////////////////////////////////////////////////////////
;<<<<<<<<<<<<<<<<<<<Put mark>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;//////////////////////////////////////////////////////////
(define (put-mark state)
  (let* ((maze (car state))
         (x (caadr state))
         (y (cadadr state))
         (facing (caddr state))
         (maze2 (increment-by-one x y 0 maze '())))
    (list maze2 (list x y) facing)
    )
  )

(define (increment-by-one-y y posc maze mazec)
  (cond
    ((null? maze) mazec)
    ((= y posc) (increment-by-one-y y (+ posc 1) (cdr maze) (append mazec (list(+ (car maze) 1)))))
    (#t (increment-by-one-y y (+ posc 1) (cdr maze) (append mazec (list (car maze)))))
    )
  )
  
(define (increment-by-one x y posc maze mazec)
  (cond
    ((null? maze) mazec)
    ((= x posc) (increment-by-one x y (+ posc 1) (cdr maze) (append mazec (list (increment-by-one-y y 0 (car maze) '())))))
    (#t (increment-by-one x y (+ posc 1) (cdr maze) (append mazec (list (car maze)))))
    )
  )

;//////////////////////////////////////////////////////////
;<<<<<<<<<<<<<<<<<<<Get mark>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;//////////////////////////////////////////////////////////
(define (get-mark state)
  (let* ((maze (car state))
         (x (caadr state))
         (y (cadadr state))
         (facing (caddr state))
         (maze2 (decrement-by-one x y 0 maze '())))
    (list maze2 (list x y) facing)
    )
  )

(define (decrement-by-one-y y posc maze mazec)
  (cond
    ((null? maze) mazec)
    ((= y posc)(cond
                 ((= (car maze) 0) (decrement-by-one-y y (+ posc 1) (cdr maze) (append mazec (list (car maze)))))
                 (#t (decrement-by-one-y y (+ posc 1) (cdr maze) (append mazec (list(- (car maze) 1)))))))
    (#t (decrement-by-one-y y (+ posc 1) (cdr maze) (append mazec (list (car maze)))))
    )
  )
  
(define (decrement-by-one x y posc maze mazec)
  (cond
    ((null? maze) mazec)
    ((= x posc) (decrement-by-one x y (+ posc 1) (cdr maze) (append mazec (list (decrement-by-one-y y 0 (car maze) '())))))
    (#t (decrement-by-one x y (+ posc 1) (cdr maze) (append mazec (list (car maze)))))
    )
  )

;************************************************************
;************************************************************
;**********************Procedure Modifiers*******************
;************************************************************
;************************************************************
(define (parse-proc li name)
  (cond
    ((eqv? (cadar li) name) (caddar li))
    (#t (parse-proc (cdr li) name))
    )
  )
  

;************************************************************
;************************************************************
;**********************Actual Procedure**********************
;************************************************************
;************************************************************
(define (simulate state expr proc limit)
  (cond
    ((list? expr) (simulate-proc state expr proc limit))
    (#t (simulate-proc state (list expr) proc limit))
    )
)
(define (simulate-proc state expr proc limit)
  (define act (car expr))
  (display act)
  (cond
    ((eqv? act 'turn-left) (simulate-proc (turn-left state) (cdr expr) proc limit))
    ((eqv? act 'step) (simulate-proc (step state) (cdr expr) proc limit))
    ((eqv? act 'put-mark) (simulate-proc (put-mark state) (cdr expr) proc limit))
    ((eqv? act 'get-mark) (simulate-proc (get-mark state) (cdr expr) proc limit))
    (#t (simulate-proc state (append (parse-proc proc act) (cdr expr)) proc limit))
    )
  )
  