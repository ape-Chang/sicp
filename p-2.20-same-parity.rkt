#lang racket

(define (same-parity . list)
  (define (even? n) (= (remainder n 2) 0))
  
  (let ((parity (even? (car list))))
    (define (recursive list)
      (if (null? list)
          '()
          (if (xor parity (even? (car list)))
              (recursive (cdr list))
              (cons (car list) (recursive (cdr list))))))
    
    (cons (car list)
          (recursive (cdr list)))))
;;
(same-parity 1 2 3 4 5 6 7)