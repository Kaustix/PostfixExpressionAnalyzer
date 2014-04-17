(define stackList '())
(define temp '())

(define (main)
  (display "UofL>")
  
  
  (define inputString(read-keyboard-as-string)) ; get the input string
  (define inputList(str-split inputString #\space)) ; convert the input string to a lis
  
  (if (pair? inputList)
      (if (not (string=? "QUIT" (car inputList)))
          (begin
            (interpret inputList) ; interpret the input list
            (main)))) ; recursievly call main to get input
)
  
(define (interpret argList)
  (define setArgList argList)
  (if (pair? setArgList)
      (begin
        
        (if (number? (string->number (car setArgList))) (push (string->number(car setArgList)))) ;push int op
  
        (if (string=? "+" (car setArgList)) (add)) ;arithmetic ops
        (if (string=? "-" (car setArgList)) (sub))
        (if (string=? "*" (car setArgList)) (mult))
        (if (string=? "/" (car setArgList)) (div))
  
        (if (string=? "DROP" (car setArgList)) (drop)) ;stack ops
        (if (string=? "POP" (car setArgList))  (pop))
        (if (string=? "SAVE" (car setArgList)) (save))
        (if (string=? "DUP" (car setArgList))  (dup))
        (if (string=? "SWAP" (car setArgList)) (swap))
        (if (string=? "STACK" (car setArgList))(stack))
        (if (string=? "CLEAR" (car setArgList))(clear))
       
        
        (if (string=? ">" (car setArgList))  ;conditional + loop ops
            (begin 
              (if (string=? "LOOP" (cadr setArgList))
                  (begin 
                    (loop setArgList)
                    (set! setArgList (cut-all setArgList "POOL")))
                  (->)))) 
              
        (if (string=? "<" (car setArgList)) (-<))
        (if (string=? ">=" (car setArgList)) (->=))
        
        (if (string=? "<=" (car setArgList)) 
            (begin
              (if (string=? "LOOP" (cadr setArgList))
                  (begin
                    (loop setArgList)
                    (set! setArgList (cut-all setArgList "POOL")))
                  (-<=))))
        
        (if (string=? "IF" (car setArgList))    ; if/else ops
            (if (pair? stackList)
                (begin
                  (if (equal? 1 (car stackList))
                      (begin (drop) (interpret (cut setArgList "IF" "ELSE"))))  ; recursively call the if statemnt
                      
                  (if (equal? 0 (car stackList))
                      (begin (drop) (interpret (cut setArgList "ELSE" "THEN"))))  ; recursively call the else statemnt
                  
                  (set! setArgList (cut-all setArgList "THEN")))))             ; cut out entire if/else/then statement

        
        
        (if (pair? setArgList)  ;display ops
            (begin
              (if (string=? "." (car setArgList))  
                  (if (pair? (cdr setArgList))
                      ;display all the string in list
                      (begin (display-list (cdr setArgList)) (newline))
                      
                      ;display the stack
                      (if (pair? stackList)
                          (begin (display (car stackList)) (newline))
                          (begin (display "") (newline)) )))
        
              (interpret (cdr setArgList)))) ;recursively call interpret until empty list

)))
        
(define (cut argList startWord endWord)
  (let ((start (+ 2 (list-index argList startWord))) (end (+ 1 (list-index argList endWord))))
    (slice argList start (- end start))))

(define (cut-all argList startWord)
  (let ((start (+ 2 (list-index argList startWord))) (end (+ 1 (list-count argList))))
    (slice argList start (- end start))))

; ========== READ-LINE/INPUT OPERTAIONS ==========
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


; ========== CUSTOM STRING / LIST OPERATIONS ==========
(define (str-split str ch) ; this function splits a string by a character and returns a list
  (if (string? str)
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
      (split 0 0)))))


(define (list-index l el)
    (if (null? l)
        -1
        (if (string=? (car l) el)
            0
            (let ((result (list-index (cdr l) el)))
                (if (= result -1)
                    -1
                    (+ result 1))))))

(define (list-quote-index l)
    (if (null? l)
        -1
        (if (equal? (string-ref (car l) (- (string-length (car l)) 1)) #\")
            0
            (let ((result (list-quote-index (cdr l))))
              (if (= result -1)
                  -1
                  (+ result 1))))))

(define (list-count lst)
  (if (null? lst)
      0
      (+ 1 (list-count (cdr lst)))))

(define get-n-items
    (lambda (lst num)
        (if (> num 0)
            (cons (car lst) (get-n-items (cdr lst) (- num 1)))
            '()))) ;'

(define slice
    (lambda (lst start count)
        (if (> start 1)
            (slice (cdr lst) (- start 1) count)
            (get-n-items lst count))))

; ========== LOOP OPERATIONS ==========
(define (loop argList)
  (define x 10))

; ========== ARITHMETIC OPERATIONS ==========
(define (add) (begin (pop) (let ((x temp)) (pop) (push (+ temp x)))))  ; + - * / the top top two elemenets fo the stack
(define (sub) (begin (pop) (let ((x temp)) (pop) (push (- temp x)))))  ; and pushes the awnser back onto the stack
(define (mult)(begin (pop) (let ((x temp)) (pop) (push (* temp x))))) 
(define (div) (begin (pop) (let ((x temp)) (pop) (push (round(/ temp x)))))) ;


; ========== CONDITIONAL OPERATION ==========
(define (->) 
  (pop)
  (let ((x temp)) 
    (pop) (save)
    (if (> temp x) (push 1) (push 0))))

(define (-<) 
  (pop)
  (let ((x temp)) 
    (pop) (save)
    (if (< temp x) (push 1) (push 0))))

(define (->=) 
  (pop)
  (let ((x temp)) 
    (pop) (save)
    (if (>= temp x) (push 1) (push 0))))

(define (-<=) 
  (pop)
  (let ((x temp)) 
    (pop) (save)
    (if (<= temp x) (push 1) (push 0))))

  
; ========== STACK OPERATIONS ==========
(define (push arg) (set! stackList (cons arg stackList))) ; pushes item onto a global stack list

(define (pop) ; sets the temp variable to top item on the stack
  (if (pair? stackList)
      (begin
        (set! temp (car stackList))
        (set! stackList (cdr stackList))))
)

(define (stack)  ; calls display list and displays a new line
  (display-list stackList)
  (newline)
) 

(define (display-list list) ; displays a list as a string of values
  (if (pair? list) 
      (begin 
        (display (car list)) 
        (display #\space) 
        (display-list (cdr list))))
)

(define (drop)    ; removes the top item of the stack
  (if (pair? stackList)
      (set! stackList (cdr stackList)))
)

(define (save) ; pushes the current item in temp to the top of the stack
  (if (number? temp)
      (push temp))
) 

(define (dup) ; duplicates the first item on the stack
   (if (pair? stackList)
       (push (car stackList)))
)

(define (swap) ; swaps the first and second item on the stack
  (if (pair? stackList)
      (begin
        (pop)
        (if (pair? stackList)
            (begin
              (let ((first-element temp))
                (pop)
                (let ((second-element temp))
                  (push first-element)
                  (push second-element)))))
      ))
 )

(define (clear) (set! stackList '())) ; resets the stack to an empty list
