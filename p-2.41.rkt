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

(define (unique-triples n)
  (flat-map (lambda (i) (flat-map (lambda (j) (map (lambda (k) (list k j i))
                                                   (enumerate 1 (- j 1))))
                                  (enumerate 2 (- i 1))))
            (enumerate 3 n)))

(define (is-sum s)
  (lambda (triple)
    (let ((first (car triple))
          (second (car (cdr triple)))
          (third (car (cdr (cdr triple)))))
      (= (+ first second third) s))))

(define (triple-sum-is s n)
  (filter (is-sum s)
          (unique-triples n)))
;;
(unique-triples 5)
(triple-sum-is 7 5)