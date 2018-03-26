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
    ((west? facing) (find-position x (- y 1) maze))
    ((east? facing) (find-position x (+ y 1) maze))
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
    ((west? facing) (list maze (list x (- y 1)) facing))
    ((east? facing) (list maze (list x (+ y 1)) facing))
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
    ((null? li) '())
    ((eqv? (cadar li) name) (if (symbol? (caddar li)) (list (caddar li)) (caddar li)))
    (#t (parse-proc (cdr li) name))
    )
  )

(define (konec bastard state)
  (define maze (car state))
  (define x (caadr state))
  (define y (cadadr state))
  (define facing (caddr state))
  (list bastard (list maze (list y x) facing)))
  

;************************************************************
;************************************************************
;**********************Actual Procedure**********************
;************************************************************
;************************************************************
(define (simulate state expr proc limit numOfSteps)
  (define maze (car state))
  (define x (caadr state))
  (define y (cadadr state))
  (define facing (caddr state))
  (cond
    ((list? expr) (simulate-proc (list maze (list y x) facing) expr proc limit '() numOfSteps))
    (#t (simulate-proc (list maze (list y x) facing) (list expr) proc limit '() numOfSteps))
    )
)
(define (simulate-proc state expr proc limit bastard numOfSteps)
  ;(display numOfSteps)
  ;(display "\n")
  (if (null? expr) (konec bastard state) (let ( ;konec
  (act (car expr)))
;(display 'STATE)
 ; (display "\t")
 ; (display state)
 ; (display "\n")
  ;(display 'EXPR)
 ; (display "\t")
;  (display expr)
;  (display "\n")
;  (display 'ACT)
 ; (display "\t")
;  (display act)
;  (display "\n")
                                           ;(display "\n")
                                           (cond
    ((= numOfSteps 0) (konec bastard state)) ;konec                                         
    ((< limit 0) (konec bastard state)) ;konec
    ((null? act) (konec bastard state)) ;konec
    ((list? act) (if (eqv? (car act) 'if) (cond
                                             ((eqv? (cadr act) 'wall?)
                                              (if (wall? state)
                                                  (simulate-proc state (append (if (symbol? (caddr act)) (list (caddr act)) (caddr act)) (cdr expr)) proc limit bastard numOfSteps)
                                                  (simulate-proc state (append (if (symbol? (cadddr act)) (list (cadddr act)) (cadddr act)) (cdr expr)) proc limit bastard numOfSteps)))
                                             ((eqv? (cadr act) 'north?)
                                              (if (north? state)
                                                  (simulate-proc state (append (if (symbol? (caddr act)) (list (caddr act)) (caddr act)) (cdr expr)) proc limit bastard numOfSteps)
                                                  (simulate-proc state (append (if (symbol? (cadddr act)) (list (cadddr act)) (cadddr act)) (cdr expr)) proc limit bastard numOfSteps)))
                                             ((eqv? (cadr act) 'mark?)
                                              (if (mark? state)
                                                  (simulate-proc state (append (if (symbol? (caddr act)) (list (caddr act)) (caddr act)) (cdr expr)) proc limit bastard numOfSteps)
                                                  (simulate-proc state (append (if (symbol? (cadddr act)) (list (cadddr act)) (cadddr act)) (cdr expr)) proc limit bastard numOfSteps))))
                                           (simulate-proc state (cdr expr) proc limit bastard numOfSteps)))
    ((eqv? act 'if) (cond
                      ((eqv? (cadr expr) 'wall?)
                       (if (wall? state)
                           (simulate-proc state (append (if (symbol? (caddr expr)) (list (caddr expr)) (caddr expr)) (cddddr expr)) proc limit bastard numOfSteps)
                           (simulate-proc state (append (if (symbol? (cadddr expr)) (list (cadddr expr)) (cadddr expr)) (cddddr expr)) proc limit bastard numOfSteps)))
                      ((eqv? (cadr expr) 'north?)
                       (if (north? state)
                           (simulate-proc state (append (if (symbol? (caddr expr)) (list (caddr expr)) (caddr expr)) (cddddr expr)) proc limit bastard numOfSteps)
                           (simulate-proc state (append (if (symbol? (cadddr expr)) (list (cadddr expr)) (cadddr expr)) (cddddr expr)) proc limit bastard numOfSteps)))
                      ((eqv? (cadr expr) 'mark?)
                       (if (mark? state)
                           (simulate-proc state (append (if (symbol? (caddr expr)) (list (caddr expr)) (caddr expr)) (cddddr expr)) proc limit bastard numOfSteps)
                           (simulate-proc state (append (if (symbol? (cadddr expr)) (list (cadddr expr)) (cadddr expr)) (cddddr expr)) proc limit bastard numOfSteps)))))
    ((eqv? act 'BRK) (simulate-proc state (cdr expr) proc (+ limit 1)  bastard numOfSteps))
    ((eqv? act 'turn-left) (simulate-proc (turn-left state) (cdr expr) proc limit (append bastard '(turn-left)) (- numOfSteps 1)))
    ((eqv? act 'step) (if (wall? state) (konec bastard state) (simulate-proc (step state) (cdr expr) proc limit (append bastard '(step)) (- numOfSteps 1))))
    ((eqv? act 'put-mark) (simulate-proc (put-mark state) (cdr expr) proc limit (append bastard '(put-mark)) (- numOfSteps 1)))
    ((eqv? act 'get-mark) (if (mark? state) (simulate-proc (get-mark state) (cdr expr) proc limit (append bastard '(get-mark)) (- numOfSteps 1)) (konec bastard state)))
    (#t (if (null? (parse-proc proc act)) (konec bastard state) (simulate-proc state (append (parse-proc proc act) '(BRK) (cdr expr)) proc (- limit 1) bastard numOfSteps)))
    )) ;konec
  ))

;************************************************************
;************************************************************
;************************************************************
;************************************************************
;************************************************************
;*****************Build up on second HW**********************
;************************************************************
;************************************************************
;************************************************************
;************************************************************

;//////////////////////////////////////////////////////////
;<<<<<<<<<<<<<<<<<<<Underdo function>>>>>>>>>>>>>>>>>>>>
;//////////////////////////////////////////////////////////
(define (underdo lst)
  (cond ((null? lst) '())
        ((pair? (car lst))
         (append (underdo (car lst))
                 (underdo (cdr lst))))
        (else (cons (car lst) (underdo (cdr lst))))))

(define (counter-of-words lst counter)
  ;(display counter)
  ;(display "\n")
  ;(display "\n")
  (cond
    ((null? lst) counter)
    ((null? (car lst)) (counter-of-words (cdr lst) counter))
    ((or (eqv? (car lst) 'procedure) (eqv? (car lst) 'if)) (counter-of-words (cdr lst) counter))
    (#t (counter-of-words (cdr lst) (+ 1 counter)))
  )
)

;//////////////////////////////////////////////////////////
;<<<<<<<<<<<<<<<<<<<Counter underdo>>>>>>>>>>>>>>>>>>>>>>>>
;//////////////////////////////////////////////////////////
(define (prg-count prg)
  (define underdoed (underdo prg))
  (counter-of-words underdoed 0)
)

;//////////////////////////////////////////////////////////
;<<<<<<<<<<<<<<<<<<<Differ in facing>>>>>>>>>>>>>>>>>>>>>>>
;//////////////////////////////////////////////////////////
(define (facing-difference fac1 fac2)
  (cond
    ((eqv? fac1 fac2) 0)
    (#t 1))
)

;//////////////////////////////////////////////////////////
;<<<<<<<<<<<<<<<<<<<Difference look>>>>>>>>>>>>>>>>>>>>>>>>
;//////////////////////////////////////////////////////////
(define (look-diff state desired-state)
  (define facing1 (caddr state)  )
  (define facing2 (caddr desired-state))
  (define x (caadr state))
  (define y (cadadr state))
  (define xx (caadr desired-state))
  (define yy (cadadr desired-state))

  (+ (facing-difference facing1 facing2)  (abs (- x xx)) (abs (- y yy)))
)

;//////////////////////////////////////////////////////////
;<<<<<<<<<<<<<<<<<<<Difference function>>>>>>>>>>>>>>>>>>>>
;//////////////////////////////////////////////////////////
(define (difference num1 num2)
  (cond
    ((eqv? num1 'w) 0) 
    (#t (abs (- num1 num2)))
    )
)

;//////////////////////////////////////////////////////////
;<<<<<<<<<<<<<<<<<<<Difference maze-row>>>>>>>>>>>>>>>>>>>>
;//////////////////////////////////////////////////////////
(define (maze-difference-row row1 row2 counter)
  (cond
    ((null? (cdr row1)) counter)
    (#t (maze-difference-row (cdr row1) (cdr row2) (+ counter (difference (car row1) (car row2)))))
  )
)

;//////////////////////////////////////////////////////////
;<<<<<<<<<<<<<<<<<<<Difference actual>>>>>>>>>>>>>>>>>>>>>>
;//////////////////////////////////////////////////////////
(define (maze-difference-actual maze1 maze2 counter)
  (cond
    ((null? (cdr maze1)) counter)
    (#t (maze-difference-actual (cdr maze1) (cdr maze2) (+ counter (maze-difference-row (car maze1) (car maze2) 0))))
  )
)

;//////////////////////////////////////////////////////////
;<<<<<<<<<<<<<<<<<<<Difference maze>>>>>>>>>>>>>>>>>>>>>>>>
;//////////////////////////////////////////////////////////
(define (maze-difference maze1 maze2)
  (maze-difference-actual maze1 maze2 0)
)

;//////////////////////////////////////////////////////////
;<<<<<<<<<<<<<<<<<<<Over threshold function>>>>>>>>>>>>>>>>
;//////////////////////////////////////////////////////////
(define (is-over-threshold results thresholds)
  (cond
    ((null? results) #f)
    ((> (car results) (car thresholds)) #t)
    (#t (is-over-threshold (cdr results) (cdr thresholds)))
  )
)

;//////////////////////////////////////////////////////////
;<<<<<<<<<<<<<<<<<<<Threshold sum>>>>>>>>>>>>>>>>>>>>>>>>>>
;//////////////////////////////////////////////////////////
(define (threshold-all-sum threshold1 threshold2 )
  (cond
    ((null? threshold1) '())
    (#t (cons (+ (car threshold1) (car threshold2)) (threshold-all-sum (cdr threshold1) (cdr threshold2))))
    )
)

;//////////////////////////////////////////////////////////
;<<<<<<<<<<<<<<<<<<<Evaluate function>>>>>>>>>>>>>>>>>>>>>>
;//////////////////////////////////////////////////////////
(define (evaluate prgs pairs thresholds stack_size)
  (define unsorted (eval-in prgs pairs thresholds stack_size '()))
  (sort unsorted)

)

;//////////////////////////////////////////////////////////
;<<<<<<<<<<<<<<<<<<<Sort function>>>>>>>>>>>>>>>>>>>>>>>>>>
;//////////////////////////////////////////////////////////
(define (sort seznam)
  (for-outside seznam '())
)

;//////////////////////////////////////////////////////////
;<<<<<<<<<<<<<<<<<<<Outside for function>>>>>>>>>>>>>>>>>>>
;//////////////////////////////////////////////////////////
(define (for-outside unsorted temp)
  (cond
    ((null? unsorted) temp)
    (#t (let* ((best         (for-inside unsorted (list '(9999 9999 9999 9999) '(()))))
                (newUnsorted  (for-remove unsorted best '()))
                (newTemp      (append temp (list best))))
              (for-outside newUnsorted newTemp)
          
        )
    )
  )  
)

;//////////////////////////////////////////////////////////
;<<<<<<<<<<<<<<Seznam consideration function>>>>>>>>>>>>>>>
;//////////////////////////////////////////////////////////
(define (first-smaller? li1 li2)
  (cond
    ((null? li1) (display "error"))
    ((> (car li1) (car li2)) #f)
    ((= (car li1) (car li2)) (first-smaller? (cdr li1) (cdr li2)))
    (#t #t)
  )
)

;//////////////////////////////////////////////////////////
;<<<<<<<<<<<<<<<<<<<Inside for function>>>>>>>>>>>>>>>>>>>>
;//////////////////////////////////////////////////////////
(define (for-inside unsorted temp)
  (cond
    ((null? unsorted) temp)
    (#t (let* ( (list1 (car unsorted))
                (numbers1 (car list1)   )
                (numbers2 (car temp )   )
                (isSmaller (first-smaller? numbers1 numbers2))
                )
              
              (cond
                (isSmaller (for-inside (cdr unsorted) list1))
                (#t(for-inside (cdr unsorted) temp )))
          )
    )
  )
)

;//////////////////////////////////////////////////////////
;<<<<<<<<<<<<<<<<<<<Remove function>>>>>>>>>>>>>>>>>>>>>>>>
;//////////////////////////////////////////////////////////
(define (for-remove unsorted best temp)
  (cond
    ((null? unsorted) temp)
    ((eqv? (car unsorted) best) (for-remove (cdr unsorted) best temp))
    (#t (for-remove (cdr unsorted) best (cons (car unsorted) temp)))
  )
)

;//////////////////////////////////////////////////////////
;<<<<<<<<<<<<<<<<<<<Inside eval function>>>>>>>>>>>>>>>>>>>
;//////////////////////////////////////////////////////////
(define (eval-in prgs pairs thresholds stack_size results)
  (cond
    ((null? prgs) results)
    (#t (let* 
          (
            (result (eval-pars-p (car prgs) pairs thresholds stack_size))
            (isResultNotOk (null? result))
            (resultCombined (list result (car prgs)))
            (newResults (cons resultCombined results))
          )
          (cond
              (isResultNotOk (eval-in (cdr prgs) pairs thresholds stack_size results))
              (#t (eval-in (cdr prgs) pairs thresholds stack_size newResults))
          )
        )
    )
  )  
)

;//////////////////////////////////////////////////////////
;<<<<<<<<<<<<<<<<<<<Eval pairs function>>>>>>>>>>>>>>>>>>>>
;//////////////////////////////////////////////////////////
(define (eval-pars-p prg pairs thresholds stack_size)
  
  (define programLength (prg-count prg))
  (define programLengthThreshold (cadddr thresholds))

  (if (> programLength programLengthThreshold) '() 
      (eval-pars-p-len prg pairs thresholds stack_size (list 0 0 programLength 0)))

)

;//////////////////////////////////////////////////////////
;<<<<<<<<<<<<<<<<<<<Len function>>>>>>>>>>>>>>>>>>>>>>>>>>>
;//////////////////////////////////////////////////////////
(define (eval-pars-p-len prg pairs thresholds stack_size thresholds-sum)
  ;(display (cadddr thresholds))
  ;(display "\n")
  (cond
    ((null? pairs) thresholds-sum)
    (#t (let* (
              (simulatedThresholds (evaluate-one prg (car pairs) stack_size (cadddr thresholds)))  
              (thresholdsSum (threshold-all-sum simulatedThresholds thresholds-sum))
              (isThresholdNotOk (is-over-threshold thresholdsSum thresholds))
              )
              (cond
                (isThresholdNotOk '())
                (#t (eval-pars-p-len prg (cdr pairs) thresholds stack_size thresholdsSum))
              )
        )
    )
  )
)

;//////////////////////////////////////////////////////////
;<<<<<<<<<<<<<<<<<<<One evaluation function>>>>>>>>>>>>>>>>
;//////////////////////////////////////////////////////////
(define (evaluate-one prg pair stack_size numStepsBastard)
  (let* (
        (desiredState (cadr pair))
        (desiredMaze (car desiredState))
        (endSimulate (simulate (car pair) 'start prg stack_size numStepsBastard))
        (endState (cadr endSimulate))
        (endMaze (car endState))
        
        (mazeDifference (maze-difference endMaze desiredMaze))
        (robotDifference (look-diff endState desiredState))
        (programLength (prg-count prg))
        (numOfSteps (length (car endSimulate)))
        )
        ;(display mazeDifference)
        ;(display "\n")
        ;(display "\n")
        ;(display robotDifference)
        ;(display "\n")
        ;(display "\n")
        ;(display programLength)
        ;(display "\n")
        ;(display "\n")
        ;(display numOfSteps)
        ;(display "\n")
        ;(display "\n")
        (list mazeDifference robotDifference 0 numOfSteps)
      )
  )
;//////////////////////////////////////////////////////////
;<<<<<<<<<<<<<<<<<<<Tests>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;//////////////////////////////////////////////////////////
