#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSCI 301, Spring 2025             ;;
;;                                   ;;
;; Lab #3                            ;;
;;                                   ;;
;; Max Dickerson                     ;;
;; W01610772                         ;;
;;                                   ;;
;; The purpose of this program is to ;;
;; begin building an interpreter for ;;
;; scheme                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;temp, del later
(define add
  (lambda (a b)
    (cond ((number? a) (+ a b))
          ((list? a) (append a b))
          (else (error "unable to add" a b)))))

(define e1  (map list
                 '(     x  y  z ls + - * cons car cdr nil list add = nil? else)
                 (list 10 20 30 (list 1 2) + - * cons car cdr '() list add = empty? #t)))


; Checks if given symbol is in environment, returns what symbol represents, else returns string error 
; Input - sym: symbol to check for, en: list of lists to check inside
; Output - value of symbol, any type
(define lookup
  (lambda (sym en)
    (cond ((not (symbol? sym)) (error "error, sym argument not a symbol")) ; errors if first arg no symbol
          ((equal? en '()) (error "error, symbol not found in environment")) ; errors if symbol not in environment
          ((equal? sym (caar en)) (cadar en)) ; ends recursion if symbol is found, returns value of symbol
          (else (lookup sym (cdr en)))))) ; continues recursion

; iterates through a list applying a proc to each entry
(define procede
  (lambda (proc lst)
    (if (not (equal? proc list))
         (if (= (length lst) 1)
                (car lst)
                (proc (car lst) (procede proc (cdr lst))))
         (if (= (length lst) 1)
             (proc (car lst))
             (cons (car lst) (procede proc (cdr lst)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;lab 4, 04/29/25

; checks if a list begins with 'if or 'cond
; Input: lst, a list or a value (always outputs false if not a list)
; Output: boolean, #t if first value of list is 'if or 'cond, #f otherwise
(define special-form?
  (lambda (lst)
    (cond ((not (list? lst)) #f) ; not a list case
          ((or (equal? (car lst) 'if) (equal? (car lst) 'cond)) #t) ; begins with 'if or 'cond case
          (else #f))))

; evals for special-form-if
; Input: list ex, in form '(boolean val-if-true val-if-false), and en environment
; Output: if boolean is true, val-if-true, if false, val-if-false, errors otherwise
(define special-form-if
  (lambda (ex en)
    (let ((arg1 (evaluate (car ex) en))) ; for convenience
      (cond ((not (boolean? arg1)) ; first arg not boolean case
             (begin
               (display arg1)
               (error "special-form-if: evaluate doesn't poop out boolean")))
            ((not (= (length ex) 3)) ; ex wrong length case
             (begin
               (display ex)
               (error "special-form-if: length of ex not 3")))
            ; fails, asks for "application: not a procedure; expected a procedure that can be applied to arguments. given: #t"
            ; basically, it wants what is in the function to be a procedure, or arg1 to be defined as a procedure
            ((arg1) (evaluate (cadr ex) en)) ; true case
            (else (evaluate (caddr ex) en)))))) ; false

(define special-form-cond
  (lambda (ex en)
    (if (special-form? ex)
        (display en)
        "o"))) 

(define evaluate-special-form
  (lambda (ex en)
    (cond ((not (special-form? ex)) (error "evaluate-special-form: ex is not a special-form"))
          ((equal? (car ex) 'if) (special-form-if (cdr ex) en))
          ((equal? (car ex) 'cond) (special-form-cond (cdr ex) en)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Input an expression (either a number or symbol) to evaluate, and an environment (list)
; Outputs an expression or an error
(define evaluate
  (lambda (ex en)
    (begin
      ;(display ex)
      (cond ((number? ex) ex)
          ((symbol? ex) (lookup ex en))
          ((special-form? ex) (evaluate-special-form ex en)) ; lab 4
          ((list? ex) ; just if, then is below
           (if (not (procedure? (evaluate (car ex) en)))
               (begin
                 ;(display (evaluate (car ex) en))
                 (error "error, first argument is not a procedure"))
               ; 
               (procede (evaluate (car ex) en) (map (lambda (x)
                      (evaluate x en))
                    (cdr ex)))))
          (else (error "undefined error"))))))

(provide lookup)
(provide procede)
(provide evaluate)
        