#lang racket

(define (cont-frac n d k)
  (define (rec i)
    (if (> i k)
        0
        (/ (n i)
           (+ (d i)
              (rec (+ i 1))))))

  (rec 1))
;;
(define (phi)
  (/ 1
     (cont-frac (lambda (i) 1.0)
                (lambda (i) 1.0)
                10000)))
(phi)