#lang racket

(define (cont-frac n d k)
  (define (iter k result)
    (if (= k 0)
        result
        (iter (- k 1)
              (/ (n k)
                 (+ (d k)
                    result)))))

  (iter k 0))
;;
(define (e-2)
  (cont-frac (lambda (i) 1.0)
             (lambda (i) (if (= (remainder i 3) 2)
                             (* (+ (/ i 3)
                                   1))
                             1))
             1000000))
(+ 2 (e-2))