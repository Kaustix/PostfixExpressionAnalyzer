(define stackList '())
(define temp '())
(define func '())
(define condition '())
(define input-string '())

(define (main)
  (display "UofL>")
  
  (define inputString(read-keyboard-as-string)) ; get the input string
  (define inputList(str-split inputString #\space)) ; convert the input string to a lis

  (if (pair? inputList)
      (if (not (string=? "QUIT" (car inputList)))
          (begin
            (set! input-string inputList)
            (interpret inputList) ; interpret the input list
            (main)))) ; recursievly call main to get input
)

(define (interpret argList)
  (define setArgList argList)
  (if (pair? setArgList)
      (begin
        
        ; -------- Number Operations  ----------------
        (if (pair? setArgList)
            (if (number? (string->number (car setArgList))) (push (string->number(car setArgList)))))
        
        ; ----------- Artithmetic Operations ---------------
        (if (pair? setArgList)
            (begin
              (if (string=? "+" (car setArgList)) (add)) 
              (if (string=? "-" (car setArgList)) (sub))
              (if (string=? "*" (car setArgList)) (mult))
              (if (string=? "/" (car setArgList)) (div))))
        
        ; ----------- Stack Operations ----------------
        (if (pair? setArgList)
            (begin
              (if (string=? "DROP" (car setArgList)) (drop)) 
              (if (string=? "POP" (car setArgList))  (pop))
              (if (string=? "SAVE" (car setArgList)) (save))
              (if (string=? "DUP" (car setArgList))  (dup))
              (if (string=? "SWAP" (car setArgList)) (swap))
              (if (string=? "STACK" (car setArgList))(stack))
              (if (string=? "CLEAR" (car setArgList))(clear))))
       
        ; ---------- Loop Operations ---------------
        (if (pair? setArgList)
            (if (string=? "LOOP" (car setArgList)) 
                (begin 
                  (set! condition (list (car input-string) (cadr input-string)))
                  (loop-op (cut setArgList "LOOP" "POOL"))
                  (set! setArgList (cut-all setArgList "POOL")))))
        
        ; ---------- Condition Opertaion ------------
        (if (pair? setArgList)
            (begin
             (if (string=? ">"  (car setArgList)) (->))
             (if (string=? "<"  (car setArgList)) (-<))
             (if (string=? ">=" (car setArgList)) (->=))
             (if (string=? "<=" (car setArgList)) (-<=))
             (if (string=? "="  (car setArgList)) (-=))))
        
        
        ; ----------- if / else Operations ---------
        (if (pair? setArgList)
            (if (string=? "IF" (car setArgList))   
                (if (pair? stackList)
                    (begin
                      (if (equal? 1 (car stackList))
                          (begin (drop) (interpret (cut setArgList "IF" "ELSE"))))  ; recursively call the if statemnt
                      
                      (if (equal? 0 (car stackList))
                          (begin (drop) (interpret (cut setArgList "ELSE" "THEN"))))  ; recursively call the else statemnt
                  
                      (set! setArgList (cut-all setArgList "THEN"))))))            ; cut out entire if/else/then statement

        
        ; ------  Function Operations -----------
        (if (pair? setArgList)
            (begin 
              (if (string=? "FUNC" (car setArgList))
                  (begin
                    (set! func (cut setArgList "FUNC" "CNUF"))
                    (display "(") (display (car func)) (display ")") (newline)
                    (set! setArgList (cut-all setArgList "CNUF"))))
              
              (if (pair? func)
                  (if (pair? setArgList)
                      (if (string=? (car func) (car setArgList))
                          (interpret (cdr func)))))))
        
        ; -------- Display Operations -------------
        (if (pair? setArgList) 
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
        

; ========== READ-LINE/INPUT OPERTAIONS ==========
(define (read-keyboard-as-string) ; this function returns keyboard input as a string
  (let ((char (read-char)))
    (if (char=? char #\newline)
        '()
        (list->string
         (let loop ((char char))
           (if (char=? char #\newline)
               '()
               (cons char (loop (read-char)))))))))


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

(define (cut argList startWord endWord)
  (let ((start (+ 2 (list-index argList startWord))) (end (+ 1 (list-index argList endWord))))
    (slice argList start (- end start))))

(define (cut-all argList startWord)
  (let ((start (+ 2 (list-index argList startWord))) (end (+ 1 (list-count argList))))
    (slice argList start (- end start))))


; ========== ARITHMETIC OPERATIONS ==========
(define (add)
  (let ((x (car stackList)) (y (cadr stackList)))
    (drop) (drop)
    (push (+ y x))))

(define (sub) 
  (let ((x (car stackList)) (y (cadr stackList)))
    (drop) (drop)
    (push (- y x))))

(define (mult)
  (let ((x (car stackList)) (y (cadr stackList)))
    (drop) (drop)
    (push (* y x))))

(define (div) 
  (let ((x (car stackList)) (y (cadr stackList)))
    (drop) (drop)
    (push (round (/ y x)))))


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

(define (-=) 
  (pop)
  (let ((x temp)) 
    (pop) (save)
    (if (= temp x) (push 1) (push 0))))

; ========== LOOP OPERATIONS ==========
(define (loop-op arg)
  (if (equal? (car stackList) 1)
      (begin
        (interpret arg)
        (loop-op2 arg))))

(define (loop-op2 arg)
  (interpret condition)
  (if (equal? (car stackList) 1)
      (begin
        (interpret arg)
        (loop-op2 arg))))
  
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
