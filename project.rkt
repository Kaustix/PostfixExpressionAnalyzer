(define stackList '())
(define temp 0)

(define (main)
  (display "UofL>")

  
  (let ((expr (read)))
    (if (equal? expr 'stop)
        'okay
        (begin (display expr) (newline) (main))
    )
   )
)

;PUSH
(define (push arg) (set! stackList (cons arg stackList)))

;POP
(define (pop) 
  (set! temp (car stackList))
  (set! stackList (cdr stackList))
)

;STACK
(define (stack) (display-stack stackList))
(define (display-stack list) 
  (if (not(null? list)) 
      (begin (display (car list)) (display #\space) (display-stack (cdr list)))
  )
)

;DROP
(define (drop) (set! stackList (cdr stackList)))

;SAVE
(define (save) (push temp))

;DUP
(define (dup) (push (car stackList)))

;SWAP
(define (swap)
  (pop)
  (let ((first-element temp))
    (pop)
    (let ((second-element temp))
      (push first-element)
      (push second-element)
    )
  )
)

;CLEAR
(define (clear) (set! stackList '()))
  
                
