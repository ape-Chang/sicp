#lang racket

(define (cons x y)
  (lambda (m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "wtf")))))

(define (car z) (z 0))
(define (cdr z) (z 1))
;;
(car (cons 3 4))
(cdr (cons 3 4))