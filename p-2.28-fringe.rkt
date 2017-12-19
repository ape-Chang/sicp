#lang racket

(define (fringe list)
  (if (null? list)
      '()
      (let ((head (car list))
            (rest (cdr list)))
        (if (pair? head)
            (append (fringe head)
                    (fringe rest))
            (cons head (fringe rest))))))
;;
(define x (list (list 1 2) (list 3 4)))
x
(fringe x)
(fringe (list x x))