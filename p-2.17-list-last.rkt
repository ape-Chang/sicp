#lang racket

(define (last-pair list)
  (let ((rest (cdr list)))
    (if (null? rest)
        (car list)
        (last-pair rest))))
;;
(last-pair (list 23 72 149 34))