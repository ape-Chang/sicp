#lang racket

(define (tree-map f tree)
  (cond ((null? tree) '())
        ((pair? tree) (cons (tree-map f (car tree))
                            (tree-map f (cdr tree))))
        (else (f tree))))
;;
(define (square x) (* x x))
(define (square-tree tree) (tree-map square tree))
;;
(define x (list 1 (list 2 (list 3 4) 5)))
x
(square-tree x)