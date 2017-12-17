#lang racket

(define (fast-expt b n)
  
  (define (square x) (* x x))

  (define (even? n) (= (remainder n 2) 0))
  
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (cons a b)
  (* (fast-expt 2 a)
     (fast-expt 3 b)))

(define (car n)
  
  (if (= (remainder n 2) 0)
      (+ 1
         (car (/ n 2)))
      0))

(define (cdr n)
  (if (= (remainder n 3) 0)
      (+ 1
         (cdr (/ n 3)))
      0))
;;
(car (cons 3 4))
(cdr (cons 3 4))