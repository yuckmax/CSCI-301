#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSCI 301, Spring 2025
;; Lab #2
;;
;; Max Dickerson
;; W01610772
;; 
;; The purpose of this lab is
;; to produce a power set of the
;; members of a list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; takes parameters e: element to be distributed, lst: list of lists
; to distribute elements amongst
; returns lst but each list inside has e as well
(define distribute
  (lambda (e lst)
    (if (equal? lst '())
        null ; base case
        ; let is for my reading convenience
        (let ((lst2 (append (list e) (car lst))))
              (append (distribute e (cdr lst)) (list lst2))))))

; takes parameter lst: list
; returns power set of all possible sets in the list
(define sublists
  (lambda (lst)
    (if (equal? lst '())
        '(()) ; base case
        ; (append A0 A1), basically
        (append (sublists (cdr lst)) (distribute (car lst) (sublists (cdr lst)))))))

; takes parameters lst1, lst2: lists of integers
; returns true if lst1's elements are less than or equal to lst2's
(define element-ordered?
  (lambda (lst1 lst2)
    (cond ((equal? lst1 lst2) #t)
          ((< (car lst1) (car lst2)) #t)
          ((> (car lst1) (car lst2)) #f)
          ; recurs to loop through list
          (else (element-ordered? (cdr lst1) (cdr lst2))))))

; takes parameters lst1, lst2: lists of integers
; returns true if lst1 is a shorter list than lst2,
; false if lst2 is a shorter list than lst1,
; element-ordered? if they're equal length
(define length-ordered?
  (lambda (lst1 lst2)
    (let ((length-lst1 (length lst1))
          (length-lst2 (length lst2))) ; let saves computing power, idk how big length is
      (cond ((< length-lst1 length-lst2) #t)
            ((> length-lst1 length-lst2) #f)
            (else (element-ordered? lst1 lst2))))))

; takes parameter lst: list representing a set of integers
; returns a power set of the set, ordered
(define subsets
  (lambda (lst)
    (sort (sublists lst)
          length-ordered?)))

(provide distribute)
(provide sublists)
(provide element-ordered?)
(provide length-ordered?)
(provide subsets)