#lang racket

(define (fast-prod a b)
  (define (zero? n)
    (= n 0))
  
  (define (even? n)
    (= (remainder n 2) 0))

  (define (double n)
    (* n 2))

  (define (halve n)
    (/ n 2))

  (cond ((zero? b) 0)
        ((even? b) (fast-prod (double a)
                              (halve b)))
        (else (+ a (fast-prod a (- b 1))))))

;;

(fast-prod 3 4)
(fast-prod 20 31)