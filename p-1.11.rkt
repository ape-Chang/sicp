#lang racket

(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))

(define (f2 n)
  (define (f2-iter a b c n)
    (if (= n 0)
        a
        (f2-iter b
                 c
                 (+ c
                    (* 2 b)
                    (* 3 a))
                 (- n 1))))
  (f2-iter 0 1 2 n))

;;
(f 10)
(f2 10)
(f 20)
(f2 20)
(f2 50)