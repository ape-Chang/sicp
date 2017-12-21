#lang racket

(define (fold-right op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op (car rest) result)
              (cdr rest))))
  (iter initial sequence))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (r-reverse seq)
  (fold-right cons '() seq))
(define (l-reverse seq)
  (fold-left (lambda (x y) (cons y x)) '() seq))
;;
(define x (list 1 2 3 4))
(l-reverse x)
(r-reverse x)