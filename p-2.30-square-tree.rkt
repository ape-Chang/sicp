#lang racket

(define (square x) (* x x))

(define (square-tree tree)
  (cond ((null?  tree) '())
        ((pair? tree) (cons (square-tree (car tree))
                            (square-tree (cdr tree))))
        (else (square tree))))

(define (x-square-tree tree)
  (map (lambda (sub-tree) (if (pair? sub-tree)
                              (x-square-tree sub-tree)
                              (square sub-tree)))
       tree))
;;
(define x (list 1 (list 2 (list 3 4) 5)))
x
(square-tree x)
(x-square-tree x)