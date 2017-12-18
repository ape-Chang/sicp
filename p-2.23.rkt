#lang racket

(define (for-each f items)
  (if (null? items)
      #t
      (let ((a 1))
        (f (car items))
        (for-each f (cdr items)))))
;;
(for-each (lambda (x) (newline) (display x))
          (list 1 2 3 4))