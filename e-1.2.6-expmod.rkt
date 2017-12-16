#lang racket

(define (expmod base exp m)
  (define (zero? n)
    (= n 0))

  (define (square n)
    (* n n))

  (define (even? n)
    (= (remainder n 2) 0))

  (cond ((zero? exp) 1)
        ((even? exp) (remainder (square (expmod base
                                                (/ exp 2)
                                                m))
                                m))
        (else (remainder (* base (expmod base
                                      (- exp 1)
                                      m))
                         m))))