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
;(define (delay exp) (memo-proc (lambda () exp)))
(define-syntax delay
  (syntax-rules ()
    ((_ exp) (memo-proc (lambda () exp)))))
(define (force delayed) (delayed))
;; definition of list-like functions of stream
(define (stream-null? s) (eq? s empty-stream))
;(define (cons-stream a b) (cons a (delay b)))
(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))
(define (stream-car s) (car s))
(define (stream-cdr s) (force (cdr s)))
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

; a much general map implementation
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      empty-stream
      (cons-stream (apply proc (map stream-car argstreams))
                   (apply stream-map (cons proc (map stream-cdr argstreams)))))) ; <- ??

;(define (stream-map proc s)
;  (if (stream-null? s)
;      empty-stream
;      (cons-stream (proc (stream-car s))
;                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))
(define (stream-filter pred s)
  (cond ((stream-null? s) empty-stream)
        ((pred (stream-car s)) (cons-stream (stream-car s)
                                           (stream-filter pred (stream-cdr s))))
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
;(stream-ref x 7)
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
(define primes (sieve (integers-starting-from 2)))
;(stream-ref primes 50)
;; ----------- define stream in a recursive way -------------
(define (add-streams s1 s2) (stream-map + s1 s2))
(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))
;; 3.53
;(define s (cons-stream 1 (add-stream s s)))
;(stream-ref s 3)
;; 3.54
(define (mul-streams s1 s2) (stream-map * s1 s2))
(define factorials (cons-stream 1 (mul-streams factorials integers)))
;(stream-ref factorials 5)
;; 3.55
(define (partial-sums s) (cons-stream (stream-car s)
                                      (add-streams (stream-cdr s) (partial-sums s))))
(define s (partial-sums integers))
;(stream-ref s 4)
;; ------------------ stream of pairs -----------------------
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))
(define (pairs s t)
  (cons-stream (list (stream-car s)
                     (stream-car t))
               (interleave
                (stream-map (lambda (x) (list (stream-car s) x))
                            (stream-cdr t))
                (pairs (stream-cdr s)
                       (stream-cdr t)))))