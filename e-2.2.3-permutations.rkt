#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flat-map proc seq)
  (accumulate append '() (map proc seq)))

(define (remove item seq)
  (filter (lambda (x) (not (= x item))) seq))

(define (permutations s)
  (if (null? s)
      (list '())
      (flat-map (lambda (x) (map (lambda (p) (cons x p))
                                 (permutations (remove x s))))
                s)))
;;
(permutations (list 1 2 3))