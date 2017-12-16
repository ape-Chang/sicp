#lang racket

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (next x) (+ x dx))

  (* dx
     (sum f a  next b)))

(define (simpson-integral f a b dx)
  (define (next x) (+ x dx))

  (* dx
     (sum f a  next b)))