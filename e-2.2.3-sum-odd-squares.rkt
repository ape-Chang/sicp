#lang racket

(define (square x) (* x x))
(define (odd? n) (= (remainder n 2) 1))

(define (sum-odd-square tree)
  (cond ((null? tree) 0)
        ((pair? tree) (+ (sum-odd-square (car tree))
                         (sum-odd-square (cdr tree))))
        (else (if (odd? tree) (square tree) 0))))