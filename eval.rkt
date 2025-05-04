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
    (begin
      ;(display proc)
      ;(display "p ")
      ;(display lst)
      ;(display "lst ")
      ;(display (car lst))
      ;(display "car ")
    (if (not (equal? proc list))
         (if (= (length lst) 1)
             (begin
               ;(display "11 ")
                (if (and (list? (car lst)) (not (equal? proc cons)))
                    (proc (car lst))
                    (car lst)))
             (begin
               ;(display "12 ")
                (proc (car lst) (procede proc (cdr lst)))))
         (if (= (length lst) 1)
             (begin
               ;(display "21 ")
               (proc (car lst)))
             (begin
               ;(display "22 ")
               (cons (car lst) (procede proc (cdr lst)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;lab 4, 04/29/25

;; 1 error now!!!!

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
    (let ((a (evaluate (car ex) en))) ; for convenience
      (cond ((not (boolean? a)) ; first arg not boolean case
             (begin
               ;(display a) ; comment out when done
               (error "special-form-if: evaluate doesn't poop out boolean")))
            ((not (= (length ex) 3)) ; ex wrong length case
             (begin
               ;(display ex) ; comment out when done
               (error "special-form-if: length of ex not 3")))
            ; if test's bugging, might need to change evaluate to
            ; just outputting cadr ex or caddr ex
            ((equal? a #t) (evaluate (cadr ex) en)) ; true case
            (else (evaluate (caddr ex) en)))))) ; false

; evals for special-form-cond
; Input: list ex of lists, last one containing an else, and en an environment
; Output: The val of the true condition, otherwise, val of else
(define special-form-cond
  (lambda (ex en)
    (if (equal? ex '()) ; check if else exists, or succeeds
        (error "cond: else doesn't work")
        (let ((a (evaluate (caar ex) en))) ; convenience, condition
          (cond ((not (boolean? a)) ; caar ex not boolean case
                 (begin
                   ;(display a)
                   (error "special-form-cond: evaluate doesn't poop out boolean")))
                ; cdar outputs a list, if cdar is length 1, needs to output cadar
                ((equal? a #t) (if (= (length (cdar ex)) 1)
                                   (evaluate (cadar ex) en)
                                   (evaluate (cdar ex) en))) ; true case
                (else (special-form-cond (cdr ex) en))))))) ; recurs until finish

; evaluates a special-form, calls special-form-if and special-form-cond
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
(provide special-form?)
(provide special-form-if)
(provide special-form-cond)
(provide evaluate-special-form)
        