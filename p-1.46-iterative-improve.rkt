#lang racket

(define (iterative-improve good-enough? improve)
  (define (rec guess)
    (if (good-enough? guess)
        guess
        (rec (improve guess))))
  
  rec)
;;
(define (sqrt x)
  ((iterative-improve (lambda (y) (< (abs (- x (* y y))) .0001))
                      (lambda (y) (/ (+ y (/ x y))
                                     2)))
   1.0))
(sqrt 2)
;;
(define (fixed-point f)
  ((iterative-improve (lambda (y) (< (abs (- y (f y))) .0001))
                      (lambda (y) (f y)))
   1.0))
(fixed-point cos)