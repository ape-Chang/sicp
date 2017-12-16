#lang racket

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))

  (define (try guess)
     (let ((next (f guess)))
       (display guess)
       (newline)
       
      (if (close-enough? guess next)
          (display next)
          (try next))))

  (try first-guess))
;;
(define (answer-me)
  (fixed-point (lambda (x) (/ (log 1000) (log x)))
               2.0))
(define (answer-me-again)
  (fixed-point (lambda (x) (/ (+ x
                                 (/ (log 1000) (log x)))
                              2))
               2.0))
(answer-me)
(newline)
(display "*********************")
(newline)
(answer-me-again)
