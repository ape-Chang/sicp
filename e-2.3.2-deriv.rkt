#lang racket

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum (make-product (multiplier exp)
                                 (deriv (multiplicand exp) var))
                   (make-product (deriv (multiplier exp) var)
                                 (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (make-product (exponent exp)
                                     (make-exponentiation (base exp) (- (exponent exp) 1)))
                       (deriv (base exp) var)))
        (else
         (error "unknow expression type -- DERIV" exp))))
;;
(define variable? symbol?)
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (sum? exp) (and (pair? exp) (eq? (car exp) '+)))
(define (addend exp) (car (cdr exp)))
(define (augend exp) (car (cdr (cdr exp))))
(define (product? exp) (and (pair? exp) (eq? (car exp) '*)))
(define (multiplier exp) (car (cdr exp)))
(define (multiplicand exp) (car (cdr (cdr exp))))
(define (=number? x n) (and (number? x) (= x n)))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        (else (list '* m1 m2))))
(define (exponentiation? exp) (and (pair? exp) (eq? (car exp) '**)))
(define (base exp) (car (cdr exp)))
(define (exponent exp) (car (cdr (cdr exp))))
(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        (else (list '** base exponent))))
;;
(deriv '(+ (** x 3) 3) 'x)