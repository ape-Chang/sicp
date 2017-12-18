#lang racket

(define (reverse list)
  (define (iter original reversed)
    (if (null? original)
        reversed
        (iter (cdr original) (cons (car original) reversed))))
  (iter list '()))
;;
(list 1 2 3 4 5)
(reverse (list 1 2 3 4 5))