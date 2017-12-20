#lang racket

(define (even? n) (= 0 (remainder n 2)))
(define (fib n)
  (define (iter k a b)
    (if (< k n)
        (iter (+ k 1) b (+ a b))
        a))
  (iter 0 0 1))

(define (even-fibs n)
  (define (next k)
    (if (> k n)
        '()
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))