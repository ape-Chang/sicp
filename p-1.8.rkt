#lang racket

;;
(define (cbrt x) (cbrt-iter 1.0 x))

(define (cbrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (cbrt-iter (improve guess x) x)))

(define (good-enough? guess x)
  (< (abs (- (* guess guess guess) x)) 0.001))

(define (improve guess x)
  (/ (+ (* 2 guess)
        (/ x (* guess guess)))
     3))

;; driver
(cbrt 1.0)
(cbrt 2.0)
(cbrt 3.0)
(cbrt 4.0)