#lang racket

(require compatibility/mlist)

(define (last-pair x)
  (let ((rest (mcdr x)))
    (if (null? rest) x (last-pair rest))))

(define (append! x y)
  (set-mcdr! (last-pair x) y)
  x)
;;
(define x (mlist 'a 'b))
(define y (mlist 'c 'd))
(define z (mappend x y))
(mlist->list z)
(mlist->list (mcdr x))
(define w (append! x y))
(mlist->list w)
(mlist->list (mcdr x))
;;
(define (make-cycle x) (set-mcdr! (last-pair x) x) x)