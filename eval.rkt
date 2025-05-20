#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSCI 301, Spring 2025                           ;;
;;                                                 ;;
;; Lab #7                                          ;;
;;                                                 ;;
;; Max Dickerson                                   ;;
;; W01610772                                       ;;
;;                                                 ;;
;; The purpose of this program is to               ;;
;; finish building an interpreter for              ;;
;; scheme, add letrec                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; lab 3 - redone 05/18/25 - Max Dickerson

; Checks if given symbol is in environment, returns what symbol represents, else returns string error 
; Input - sym: symbol to check for, en: list of lists to check inside
; Output - value of symbol, any type
(define lookup
  (lambda (sym en)
    (cond ((not (symbol? sym)) (error "lookup: sym is not a symbol"))
          ((equal? en '()) (error "lookup: sym not found in en")) ; errors if symbol not in environment
            ((equal? sym (caar en)) (cadar en)) ; ends recursion if symbol is found, returns value of symbol
            (else (lookup sym (cdr en)))))) ; continues recursion

; Input an expression (either a number or symbol) to evaluate, and an environment (list)
; Outputs an expression or an error
(define evaluate
  (lambda (expr en)
    (cond ((number? expr) expr)
          ((symbol? expr) (lookup expr en))
          ((special-form? expr) (evaluate-special-form expr en)) ; lab 4
          ((list? expr) ; lab 6
           (apply-function (evaluate (car expr) en) (map (lambda (x) (evaluate x en)) (cdr expr))))
          (else (error "evaluate - else: undefined error")))))

(provide lookup)
(provide evaluate)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; lab 4 - redone 05/18/25 - Max Dickerson

; checks if input is special form
; Input - list
; Output - false if lst not list, or if (car lst) is not 'if,'cond,'let,'lambda, otherwise true
(define special-form?
  (lambda (lst)
    (cond ((not (list? lst)) #f)
          ((or (equal? (car lst) 'if)
               (equal? (car lst) 'cond)
               ; lab 5:
               (equal? (car lst) 'let)
               ; lab 6:
               (equal? (car lst) 'lambda)
               ; lab 7
               (equal? (car lst) 'letrec))) 
          (else #f))))

; evals for special-form-if
; Input: list ex, in form '(boolean val-if-true val-if-false), and en environment
; Output: if boolean is true, val-if-true, if false, val-if-false, errors otherwise
(define special-form-if
  (lambda (expr en)
    (let ((a (evaluate (car expr) en))) ; for convenience
      (cond ((not (boolean? a)) (error "special-form-if: evaluate doesn't poop out boolean"))
            ((not (= (length expr) 3)) (error "special-form-if: length of ex not 3"))
            ((equal? a #t) (evaluate (cadr expr) en)) ; true case
            (else (evaluate (caddr expr) en)))))) ; false

; evals for special-form-cond
; Input: list ex of lists, last one containing an else, and en an environment
; Output: The val of the true condition, otherwise, val of else
(define special-form-cond
  (lambda (expr en)
    (if (equal? expr '()) ; check if else exists, or succeeds
        (error "cond: else doesn't work")
        (let ((a (evaluate (caar expr) en))) ; convenience, condition
          (cond ((not (boolean? a)) (error "special-form-cond: evaluate doesn't poop out boolean"))
                ; cdar outputs a list, if cdar is length 1, needs to output cadar
                ((equal? a #t) (if (= (length (cdar expr)) 1)
                                   (evaluate (cadar expr) en)
                                   (evaluate (cdar expr) en))) ; true case
                (else (special-form-cond (cdr expr) en))))))) ; recurs until finish

; evaluates special form, redirects to special-form-if, special-form-cond, special-form-let
; Input - expression, environment
; Output - error if not special form, otherwise evaluates the special form
(define evaluate-special-form
  (lambda (expr en)
    (cond ((not (special-form? expr)) (error "evaluate-special-form: expr is not a special form"))
          ((equal? (car expr) 'if) (special-form-if (cdr expr) en))
          ((equal? (car expr) 'cond) (special-form-cond (cdr expr) en))
          ((equal? (car expr) 'let) (special-form-let (cadr expr) (caddr expr) en en)) ; lab 5
          ((equal? (car expr) 'lambda) (closure (cadr expr) (caddr expr) en)) ; lab 6
          ((equal? (car expr) 'letrec) (special-form-letrec (cadr expr) (caddr expr) en en)) ; lab 7
          (else (error "evaluate-special-form: else case")))))

(provide special-form?)
(provide special-form-if)
(provide special-form-cond)
(provide evaluate-special-form)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; lab 5 - copy-paste 05/19/25 - Max Dickerson

; evals for special-form-let
; input: list lst of lists of pairs '(sym1 exp1), expression expr
; to be expressed with sym in lst, list old-en the environment, list new-en
; the added environment for recursion
; output: output of expr using syms defined in lst and old-en
(define special-form-let
  (lambda (lst expr old-en new-en)
    (if (equal? lst '())
        (evaluate expr new-en) ; base case is just value of expr
        ; recurs through lst, keeps expr and old-en, new-en gets (sym1 exp1) in lst appended
        (special-form-let (cdr lst) expr old-en (append (list (list (caar lst) (evaluate (cadar lst) old-en))) new-en)))))

(provide special-form-let)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; lab 6 - redone 05/19/25 - Max Dickerson

; closure data-type
;(define closure (lambda (vars body env) (list 'closure vars body env)))
;(define closure? (lambda (clos) (and (pair? clos) (eq? (car clos) 'closure))))
;(define closure-vars cadr)
;(define closure-body caddr)
;(define closure-env cadddr)

; applies a procedure or closure to the rest of a list
; Input - former = evaluated car of a list, either a closure or a procedure,
; latter = evaluated cdr of a list
; Output - application of the function, errors if former is not a procedure or closure
(define apply-function
  (lambda (former latter) ; former is car, latter is cdr
    (cond ((procedure? former) (apply former latter))
          ((closure? former) (apply-closure former latter))
          (else (error "apply-function: unknown function type")))))

; adds '(clos-vars args) to local environment, evaluates clos-body in that environment
; Input - clos = a closure, vals = list of values to apply
; Output - evaluates the closure based on the values.
(define apply-closure
  (lambda (clos vals)
    (evaluate (closure-body clos) (append (map list (closure-vars clos) vals) (closure-env clos)))))

(provide closure)
(provide closure?)
(provide closure-vars)
(provide closure-body)
(provide closure-env)
(provide apply-function)
(provide apply-closure)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; lab 7 - finished 05/20/25 - Max Dickerson

; mutable closure data-type lab 7 ver.
(define closure
  (lambda (vars body env)
    (mcons 'closure (mcons env (mcons vars body)))))
(define closure?
  (lambda (clos) (and (mpair? clos) (eq? (mcar clos) 'closure))))
(define closure-env
  (lambda (clos) (mcar (mcdr clos))))
(define closure-vars
  (lambda (clos) (mcar (mcdr (mcdr clos)))))
(define closure-body
  (lambda (clos) (mcdr (mcdr (mcdr clos)))))
(define set-closure-env!
  (lambda (clos new-env) (set-mcar! (mcdr clos) new-env)))

; evaluates the letrec special form
; Input - same as let special form
; Output - evaluates the expr using functions inside a letrec
(define special-form-letrec
  (lambda (lst expr old-en new-en)
    (if (equal? lst '())
        ; moves to helper function once done letting
        (letrec-helper expr new-en '() new-en old-en)
        ; recurs through lst, keeps expr and old-en, new-en gets (sym1 exp1) in lst appended, just like let
        (special-form-letrec (cdr lst) expr old-en (append (list (list (caar lst) (evaluate (cadar lst) old-en))) new-en)))))

; helps with the second half (changing closures for lambdas and evaluate) of letrec function
; Input - expr: expression to evaluate, temp: new-en that gets recurred through,
; mini-en: an environment that starts with an empty list, new-en: environment created in letrec,
; old-en: environment outside of let form
; Output: evaluates the expr using functions inside a letrec
(define letrec-helper
  (lambda (expr temp mini-en new-en old-en)
    (if (equal? temp old-en)
        (evaluate expr (append mini-en old-en)) ; base case
        (begin
          (set-closure-env! (cadar temp) new-en) ; sets closure env
          (letrec-helper expr (cdr temp) (append mini-en (list (car temp))) new-en old-en))))) ; recurs through temp, creating mini-en in the process

(provide set-closure-env!)
(provide special-form-letrec)
(provide letrec-helper)