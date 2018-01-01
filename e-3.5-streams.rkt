#lang racket
;; empty-stream is just a special marker
(define empty-stream '())
;; definition of delay and force,
;; including an optimization mentioned.
(define (memo-proc proc)
  (let ((already-run? #f)
        (result #f))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? #t)
                 result)
          result))))
(define (delay exp) (memo-proc (lambda () exp)))
(define (force delayed) (delayed))
;; definition of list-like functions of stream
(define (stream-null? s) (eq? s empty-stream))
(define (cons-stream a b) (cons a (delay b)))
(define (stream-car s) (car s))
(define (stream-cdr s) (force (cdr s)))
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref s (- n 1))))

; a much general map implementation
;(define (stream-map proc . argstreams)
;  (if (stream-null? (car argstreams))
;      empty-stream
;      (cons-stream (apply proc (map stream-car argstreams))
;                   (apply stream-map (cons proc (map stream-cdr argstreams)))))) ; <- ??

(define (stream-map proc s)
  (if (stream-null? s)
      empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))
(define (stream-filter pred s)
  (cond ((stream-null? s) empty-stream)
        ((pred (stream-car s) (cons-stream (stream-car s)
                                           (stream-filter pred (stream-cdr s)))))
        (else (stream-filter pred (stream-cdr s)))))
;; other aux functions
(define (display-line x)
  (newline)
  (display x))
(define (display-stream s)
  (stream-for-each display-line s))
(define (stream-enumerate-interval low high)
  (if (> low high)
      empty-stream
      (cons-stream low (stream-enumerate-interval (+ low 1) high))))
;; problems of 3.5.1
;(define (show x) (display-line x) x)
;(define x (stream-map show (stream-enumerate-interval 0 10)))
;(stream-ref x 5)
;(define x (stream-map show (stream-enumerate-interval 0 10)))
; (stream-ref x 5)
;; ----------------- infinite stream -----------------------
(define (divisible? x y) (= 0 (remainder x y)))
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define (sieve stream)
  (cons-stream (stream-car stream)
               (sieve (stream-filter (lambda (x) (not (divisible? x (stream-car stream))))
                                     (stream-cdr stream)))))
(cons-stream 1 2)
;(define x (integers-starting-from 2))
;(define primes (sieve (integers-starting-from 2)))
;(stream-ref primes 5)