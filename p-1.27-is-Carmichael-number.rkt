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

(define (is-Carmichael-number n)

  (define (test-Carmichael n a)
    (cond ((= n a) #t)
          ((= (expmod a n n) a) (test-Carmichael n (+ a 1)))
          (else #f)))

  (test-Carmichael n 2))

;;

(is-Carmichael-number 561)
(is-Carmichael-number 1105)
(is-Carmichael-number 1729)
(is-Carmichael-number 2465)
(is-Carmichael-number 2821)
(is-Carmichael-number 6601)
(is-Carmichael-number 20)
(is-Carmichael-number 37)