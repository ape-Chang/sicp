#lang racket
(require "common.rkt")

(define (esqrt x) (sqrt-iter 1.0 x))
;; this will result an endless loop till out of memory
;; for else-clause will always be evaluated, before cond
;; expression be evaluated
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.0001))

;; driver case
(esqrt 2)