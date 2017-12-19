#lang racket

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s)))
            (head (car s)))
        (append rest (map (lambda (x) (cons head x))
                          rest)))))
;;
(subsets (list 1 2 3))