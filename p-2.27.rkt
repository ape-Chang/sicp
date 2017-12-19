#lang racket

(define (reverse list)
  (define (iter original reversed)
    (if (null? original)
        reversed
        (iter (cdr original) (cons (car original) reversed))))
  (iter list '()))

(define (deep-reverse list)
  (define (iter original reversed)
    (if (null? original)
        reversed
        (let ((head (car original))
              (rest (cdr original)))
          (if (pair? head)
              (iter rest
                    (cons (deep-reverse head) reversed))
              (iter rest
                    (cons head reversed))))))
  (iter list '()))
;;
(define x (list (list 1 2) (list 3 4)))
x
(reverse x)
(deep-reverse x)