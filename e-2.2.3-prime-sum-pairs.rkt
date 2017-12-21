#lang racket

(define (enumerate low high)
  (if (> low high)
      '()
      (cons low (enumerate (+ low 1) high))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flat-map proc seq)
  (accumulate append '() (map proc seq)))

(define (pairs n)
  (accumulate append
              '()
              (map (lambda (i) (map (lambda (j) (list j i))
                                    (enumerate 1 (- i 1))))
                   (enumerate 1 n))))

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

(define (prime-sum? pair)
  (is-prime? (+ (car pair)
                (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair)
                                  (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (pairs n))))
;;
(pairs 5)
(prime-sum-pairs 5)