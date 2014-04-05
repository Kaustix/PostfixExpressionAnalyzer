(define stackList '())
(define temp 0)

(define (main)
  (display "UofL>")
  
  (define inputString(read-keyboard-as-string)) ; get the input string
  (define inputList(str-split a #\space)) ; convert the input string to a list
  
  (interpret inputList) ; interpret the input list

  (main) ; recursivly call main
)

(define (interpret arg)
  (if (
  
)

(define (read-keyboard-as-string) ; this function returns keyboard input as a string
  (let ((char (read-char)))
    (if (char=? char #\newline)
        '()
        (list->string
         (let loop ((char char))
           (if (char=? char #\newline)
               '()
               (cons char (loop (read-char)))
               )
           )
         )
        )
    )
)

(define (str-split str ch) ; this function splits a string by a character and returns a list
  (let ((len (string-length str)))
    (letrec
      ((split
        (lambda (a b)
          (cond
            ((>= b len) (if (= a b) '() (cons (substring str a b) '())))
            ((char=? ch (string-ref str b)) (if (= a b)
                                              (split (+ 1 a) (+ 1 b))
                                              (cons (substring str a b) (split b b))))
            (else (split a (+ 1 b)))))))
      (split 0 0))))

(define (push arg) (set! stackList (cons arg stackList))) ; pushes item onto a global stack list

(define (pop) ; sets the temp variable to top item on the stack
  (set! temp (car stackList))
  (set! stackList (cdr stackList))
)

(define (stack) (display-list stackList)) ; displays the stack list as seperate string values
(define (display-list list) 
  (if (not(null? list)) 
      (begin (display (car list)) (display #\space) (display-list (cdr list)))
  )
)

(define (drop) (set! stackList (cdr stackList))) ; removes the top item of the stack

(define (save) (push temp)) ; pushes the current item in temp to the top of the stack

(define (dup) (push (car stackList))) ; duplicates the first item on the stack

(define (swap) ; swaps the first and second item on the stack
  (pop)
  (let ((first-element temp))
    (pop)
    (let ((second-element temp))
      (push first-element)
      (push second-element)
    )
  )
)

(define (clear) (set! stackList '())) ; resets the stack to an empty list
