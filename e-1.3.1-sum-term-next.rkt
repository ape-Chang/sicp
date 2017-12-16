#lang racket

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (sum-cubes a b)
  (define (cube n) (* n n n))
  (define (inc n) (+ n 1))

  (sum cube a inc b))

(define (sum-integers a b)
  (define (id n) n)
  (define (inc n) (+ n 1))

  (sum id a inc b))

(define (pi-sum a b)
  (define (term n) (/ 1.0 (* n (+ n 2))))
  (define (next n) (+ n 4))

  (sum term a next b))

(define (integral f a b dx)
  (define (next x) (+ x dx))

  (* dx
     (sum f a  next b)))

;;

(sum-cubes 1 10)
(sum-integers 1 10)
(* 8 (pi-sum 1 100000))
(integral (lambda (x) (* x x x))
          0
          1
          0.001)