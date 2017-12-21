#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product w v)) m))

(define (transpose m)
  (accumulate-n cons '() m))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v) (matrix-*-vector cols v)) m)))
;;
(define v (list 1 2 3 4))
(define w (list 5 6 7 8))
(define m (list (list 1 2 3 4)
                (list 1 2 3 4)
                (list 1 2 3 4)
                (list 1 2 3 4)))
(dot-product v w)
(matrix-*-vector m v)
(transpose m)
(matrix-*-matrix m (transpose m))