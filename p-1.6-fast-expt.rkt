#lang racket

;; FIXME

(define (fast-expt b n)

  (define (even? n)
    (= (remainder n 2) 0))

  (define (fe-iter n e)
    (if (= n 0)
        e
        (if (even? n)
            (fe-iter (/ n 2)
                     (* e e))
            (fe-iter (- n 1)
                     (* e b)))))

  (fe-iter n 1))

;;
(fast-expt 2 10)