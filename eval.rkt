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
;; yadda yadda                       ;;
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
    (cond ((not (symbol? sym)) "error, sym argument not a symbol") ; errors if first arg no symbol
          ((equal? en '()) "error, symbol not found in environment") ; errors if symbol not in environment
          ((equal? sym (caar en)) (cadar en)) ; ends recursion if symbol is found, returns value of symbol
          (else (lookup sym (cdr en)))))) ; continues recursion

(define evaluate
  (lambda (ex en)
    (cond ((number? ex) ex)
          ((symbol? ex) (lookup ex en))
          ((list? ex) ; just if, then is below
           (if (not (procedure? (lookup (car ex) en)))
               "error, first argument is not a procedure"
               ; what fucks me over is down here, can't recur through cdr because
               ; cdr doesn't start with procedure
               ; map: all lists must be the same size
               ; rn this outputs the cdr of ex added to itself, needs to output total of expression
               (map
                (lambda (x)
                  ((lookup (car ex) en) x (evaluate x en)))
                (cdr ex))))
          (else "undefined error"))))
               
          
        