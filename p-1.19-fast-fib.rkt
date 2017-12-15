#lang racket

(define (fast-fib n)
  (define (zero? n)
    (= n 0))

  (define (even? n)
    (= (remainder n 2) 0))
  
  (define (ff-iter a b p q n)
    (cond ((zero? n) b)
          ((even? n) (ff-iter a
                              b
                              (+ (* p p)
                                 (* q q))
                              (+ (* p q 2)
                                 (* q q))
                              (/ n 2)))
          (else (ff-iter (+ (* b q)
                            (* a q)
                            (* a p))
                         (+ (* b p)
                            (* a q))
                         p
                         q
                         (- n 1)))))

  (ff-iter 1 0 0 1 n))
;;
(fast-fib 1000)