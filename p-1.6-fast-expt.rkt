#lang racket

;;

(define (fast-expt b n)
  (define (zero? n)
    (= n 0))

  (define (even? n)
    (= (remainder n 2) 0))

  (define (fe-iter n b e)
    (cond ((zero? n) e)
          ((even? n) (fe-iter (/ n 2)
                              (* b b)
                              e))
          (else (fe-iter (- n 1)
                         b
                         (* e b)))))

  (fe-iter n b 1))

;;
(fast-expt 2 10)
(fast-expt 2 8)