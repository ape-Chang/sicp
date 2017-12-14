#lang racket

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (fib2 n)
  (define (fib2-iter a b count)
    (if (= count 0)
        b
        (fib2-iter (+ a b)
                   a
                   (- count 1))))
  (fib2-iter 1 0 n))

;;
(fib2 100)