#lang racket

(define (is-prime? n)

  (define (smallest-divisor n)

    (define (divides? a b) (= (remainder a b) 0))

    (define (square n) (* n n))
    
    (define (find-divisor n test-divisor)
      (cond ((> (square test-divisor) n) n)
            ((divides? n test-divisor) test-divisor)
            (else (find-divisor n
                                (+ test-divisor 1)))))

    (find-divisor n 2))

  (= (smallest-divisor n) n))

;;

(is-prime? 43)
(is-prime? 87)
(is-prime? 83)