#lang racket
;; aobsolutely nothing
(define painter '())
(define (flip-vert painter) '())
(define (flip-horiz painter) '())
(define (beside painter1 painter2) '())
(define (below painter1 painter2) '())
(define (identity painter) '())
;;
(define (corner-split painter) '())
;;
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (beside (below (bl painter) (tl painter))
            (below (br painter) (tr painter)))))

(define (flipped-pair painter)
  ((square-of-four identity flip-vert
                   identity flip-vert)
   painter))

(define (square-limit painter n)
  ((square-of-four flip-horiz
                   identity
                   (lambda (painter) (flip-horiz (flip-vert painter)))
                   flip-vert) (corner-split painter n)))

(define (split op1 op2)
  (define (op painter n)
    (if (= n 0)
        (identity painter)
        (let ((smaller (op painter (- n 1))))
          (op1 painter (op2 smaller smaller)))))
  op)

(define right-split (split beside below))
(define up-split (split below beside))