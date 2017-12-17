#lang racket


(define (gcd a b)
  (define (_ a b)
    (if (= b 0) a (_ b (remainder a b))))

  (cond ((> a b) (_ a b))
        ((< a b) (_ b a))
        (else a)))

(define (make-rat n d)
  (cond ((< d 0) (make-rat (- n) (- d)))
        ((> d 0) (let ((g (if (> n 0)
                              (gcd n d)
                              (gcd (- n) d))))
                   (cons (/ n g) (/ d g))))
        (else error "d is 0")))

(define (numer r) (car r))

(define (denom r) (cdr r))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (denom x) (numer y)))
            (* (denom x)
               (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (denom x) (numer y)))
            (* (denom x)
               (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x)
               (numer y))
            (* (denom x)
               (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x)
               (denom y))
            (* (denom x)
               (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (denom x) (numer y))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))
;;
(print-rat (make-rat -4 -6))
(print-rat (make-rat 4 -6))
(print-rat (make-rat 6 4))
(print-rat (make-rat -6 4))
(print-rat (make-rat -6 -4))