#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSCI 301, Spring 2025
;;
;; Lab #8
;;
;; Max Dickerson
;;
;; Grammar:
;; E -> DN | AS | (L)
;; L -> _L | EL | e
;; S -> AS | e
;; N -> DN | e
;; D -> 0..9     Dgt
;; A -> a..      Sym
;;
;; e is empty err is error
;; Predict
;;       Dgt  Sym  (   )    _    $
;; L ->  EL   EL   EL  e    _L   e
;; E ->  DN   AS   (L) err  err  err
;; S ->  e    AS   e   e    e    e
;; N ->  DN   e    e   e    e    e
;; D ->  Dgt  err  err err  err  err
;; A ->  err  Sym  err err  err  err
;;
;; Each function takes a list of the unprocessed input characters and returns
;; a list with the value parsed as the first element followed by the un-parsed
;; input.  
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide parse E L N D A S)
(require racket/trace)

;; Method for composing syntax error messages
;;
(define syntax-error-msg
  (lambda (msg input)
    (string-append "Syntax Error ("
                   msg
                   "): " (list->string input))
    )
  )


;; Main parsing function
(define parse
  (lambda (str)
    (first (L (string->list str)))
    )
  )


;; L
;;
;;       Dgt  Sym  (   )    _    $
;; L ->  EL   EL   EL  e    _L   e
(define L
  (lambda (input) '()))
                       



;; E
;;
;;       Dgt  Sym  (   )    _    $
;; E ->  DN   AS   (L) err  err  err
(define E
  (lambda (input) '()))


;; N
;;
;;       Dgt  Sym  (   )    _    $
;; N ->  DN   e    e   e    e    e
(define N
  (lambda (input inhert)
    ; might also be: if not char-numeric then (if char-whitespace then inhert else '()) else recur
    (if (char-whitespace? (car input))
        inhert ; output sym or maybe '()
        (N (cdr input) (+ (car input) (* 10 inhert)))))) ; latter equation may need to be flipped somehow | currently: 10 * inhert + (car input)
     
;; D
;;
;; D ->  Dgt  err  err err  err  err
(define D
  (lambda (input)
    (if (not (char-numeric? input))
        (error "D: input is not numeric")
        (char->number input))))

    
;; S
;;
;; S ->  e    AS   e   e    e    e
(define S
  (lambda (input sym)
    ; might also be: if not char-symbolic then (if char-whitespace then sym else '()) else recur
    (if (char-whitespace? (car input))
        (string->symbol (list->string sym)) ; output sym or maybe '()
        (S (cdr input) (append (list (A (car input))) sym))))) ; append may need to be flipped
        


;; A
;;
;; A ->  err  Sym  err err  err  err
(define A
  (lambda (input)
    (if (not (char-symbolic? input))
        (error "A: input is not a symbol")
        input)))


;; Char to Number
;; Converts a char containing a digit to the integer value.  For example
;; (char->number #\4) => 4.
(define char->number
  (lambda (char)
    (- (char->integer char)
       (char->integer #\0))))

;; Char-Symbolic
;; This is a predicate function to test if a symbol is part of a symbol. 
(define char-symbolic?
  (lambda (char)
    (and (not (char-whitespace? char))
         (not (eq? char #\())
         (not (eq? char #\))))))

;; Char-Numeric
;;
;; Note that the char-numeric? function is builtin

;; uncomment the following to see the traced execution of the parser
;(trace parse)
;(trace E)
;(trace L)
;(trace N)
;(trace D)
;(trace A)
;(trace S)
