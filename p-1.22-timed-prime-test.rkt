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

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))

  (try-it (+ 1
             (random (- n 1)))))

(define (is-prime? n)
  (define (fast-is-prime? n times)
    (cond ((= times 0) true)
          ((fermat-test n) (fast-is-prime? n (- times 1)))
          (else false)))

  (fast-is-prime? n 100))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-inexact-milliseconds)))

(define (start-prime-test n start-time)
  (if (is-prime? n)
      (report-prime (- (current-inexact-milliseconds) start-time))
      (display "")))

(define (report-prime elapsed-time)
  (display "  ***  ")
  (display elapsed-time))

;;

(timed-prime-test 199)
(timed-prime-test 1999)
(timed-prime-test 19999)