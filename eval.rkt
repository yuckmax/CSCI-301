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

; temp
(define env (list (list 'x 5)
                  (list '+ +)
                  (list '* *)))

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

; Input an expression (either a number or symbol) to evaluate, and an environment (list)
; Outputs an expression or an error
(define evaluate
  (lambda (ex en)
    (begin
      ;(display ex)
      (cond ((number? ex) ex)
          ((symbol? ex) (lookup ex en))
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
        