#lang racket

(require compatibility/mlist)

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((t (mcdr x)))
          (set-mcdr! x y)
          (loop t x))))
  (loop x '()))

(define v (mlist 'a 'b 'c 'd))
(mlist->list (mystery v))