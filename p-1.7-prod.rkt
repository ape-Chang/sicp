#lang racket

(define (prod a b)
  (if (= b 0)
      0
      (+ a
         (prod a (- b 1)))))

;;

(prod 3 5)
(prod 20 31)