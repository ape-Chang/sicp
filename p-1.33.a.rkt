#lang racket

(define (filtered-accumulate combiner null-value pred term a next b)
  (if (> a b)
      null-value
      (combiner (if (pred a)
                    (term a)
                    null-value)
                (filtered-accumulate combiner null-value pred term (next a) next b))))

(define (is-prime? n)

  (define (smallest-divisor n)

    (define (divides? a b) (= (remainder a b) 0))

    (define (square n) (* n n))
    
    (define (find-divisor n test-divisor)
      (cond ((> (square test-divisor) n) n)
            ((divides? n test-divisor) test-divisor)
            (else (find-divisor n
                                (+ test-divisor 1)))))

    (find-divisor n 2))

  (= (smallest-divisor n) n))

(define (sum-of-prime a b)
  (filtered-accumulate + 0 is-prime? (lambda (n) n) a (lambda (n) (+ n 1)) b))

;;
(sum-of-prime 3 10)