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

  (define (fp-iter n a s)
    (cond ((zero? n) s)
          ((even? n) (fp-iter (halve n)
                              (double a)
                              s))
          (else (fp-iter (- n 1)
                         a
                         (+ s a)))))

  (fp-iter b a 0))

;;

(fast-prod 3 4)
(fast-prod 20 31)