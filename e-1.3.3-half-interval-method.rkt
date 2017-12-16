#lang racket

(define (half-interval-method f a b)
  (define (negative? x) (< x 0))
  (define (positive? x) (> x 0))

  (define (search f neg-point pos-point)
    (define (close-enough? x y)
      (< (abs (- x y)) 0.001))
    
    (let ((mid-point (/ (+ neg-point pos-point)
                        2)))
      (if (close-enough? neg-point pos-point)
          mid-point
          (let ((test-value (f mid-point)))
            (cond ((negative? test-value)
                   (search f mid-point pos-point))
                  ((positive? test-value)
                   (search f neg-point mid-point))
                  (else mid-point))))))
  
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (positive? a-value) (negative? b-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))
;;
(half-interval-method sin 2.0 4.0)
(half-interval-method (lambda (x) (- (* x x x) (* x 2) 3))
                      1.0
                      2.0)