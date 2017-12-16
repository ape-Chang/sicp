#lang racket

(define (filtered-accumulate combiner null-value pred term a next b)
  (if (> a b)
      null-value
      (combiner (if (pred a)
                    (term a)
                    null-value)
                (filtered-accumulate combiner null-value pred term (next a) next b))))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (prod-of-coprime n)
  (filtered-accumulate *
                       1
                       (lambda (k) (= (gcd n k) 1))
                       (lambda (k) k)
                       1
                       (lambda (k) (+ k 1))
                       (- n 1)))
;;
(prod-of-coprime 10)