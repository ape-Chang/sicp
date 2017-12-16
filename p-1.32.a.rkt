#lang racket

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) b))))

(define (sum term a next b)
  (accumulate + 0.0 term a next b))

(define (product term a next b)
  (accumulate * 1.0 term a next b))