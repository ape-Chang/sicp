#lang racket

(define (average x y) (/ (+ x y) 2))
(define (square x) (* x x))

;; compute square root using Newton method
(define (esqrt x) (sqrt-iter 1.0 x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.0001))

;; driver case
(esqrt 2)
(esqrt 200)
(esqrt 20000)