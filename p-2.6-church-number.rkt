#lang racket

(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
(define one
  (lambda (f) (lambda (x) (f x))))
(define two
  (lambda (f) (lambda (x) (f (f x)))))
(define (plus a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))
;;
(cos 4)
((one cos) 4)
(cos (cos 4))
((two cos) 4)
(((plus one one) cos) 4)