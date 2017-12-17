#lang racket

(define (square x) (* x x))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (distance p q)
  (sqrt (+ (square (- (x-point p)
                      (x-point q)))
           (square (- (y-point p)
                      (y-point q))))))

(define (make-segment start end) (cons start end))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))
(define (segment-length segment)
  (distance (start-segment segment)
            (end-segment segment)))

(define (make-rectangle h-seg v-seg) (cons h-seg v-seg))
(define (h-segment rectangle) (car rectangle))
(define (v-segment rectangle) (cdr rectangle))
(define (circumference rectangle)
  (* (+ (segment-length (h-segment rectangle))
        (segment-length (v-segment rectangle)))
     2))
(define (area rectangle)
  (* (segment-length (h-segment rectangle))
     (segment-length (v-segment rectangle))))
;;
(distance (make-point 0 0)
          (make-point 3 4))