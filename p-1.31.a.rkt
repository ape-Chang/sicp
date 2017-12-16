#lang racket

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (factorial n)
  (define (term n) n)
  (define (next n) (+ n 1))
  
  (product term 1 next n))

(define (x-pi n)
  (define (term x) (/ (* (- x 1)
                         (+ x 1))
                      (* x x)))
  (define (next x) (+ x 2))

  (* 4
     (product term 3.0 next n)))

;;
(factorial 4)
(factorial 10)
(x-pi 100000)