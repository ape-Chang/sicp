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

(define (unique-pairs n)
  (accumulate append
              '()
              (map (lambda (i) (map (lambda (j) (list j i))
                                    (enumerate 1 (- i 1))))
                   (enumerate 1 n))))