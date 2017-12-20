#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (hornor-eval x coefficient-sequence)
  (accumulate (lambda (this-coefficient higher-terms) (+ this-coefficient (* higher-terms x)))
              0
              coefficient-sequence))
;;
(hornor-eval 2 (list 1 3 0 5 0 1))