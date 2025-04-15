#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSCI 301, Spring 2025
;; Lab #2
;;
;; Max Dickerson
;; W01610772
;; 
;; The purpose of this lab is...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define A1
  (lambda (lst)
    (if (= (cdr lst) '())
        lst
        ; use lab2 write-up as reference
        )))

; errors mysteriously because of let
(define distribute
  (lambda (e lst)
    (if (= lst '())
        '(())
        (let ((lst2 (list e))
              (list (append lst2 (car lst)) (distribute e (cdr lst))))))))

(define sublists
  (lambda (lst)
    (if (= lst '())
        '(())
        (begin
          ))))

; (provide function) for all functions in program