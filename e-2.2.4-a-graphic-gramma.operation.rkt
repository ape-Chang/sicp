#lang racket
;; aobsolutely nothing
(define painter '())
(define (flip-vert painter) '())
(define (flip-horiz painter) '())
(define (beside painter1 painter2) '())
(define (below painter1 painter2) '())
(define (identity painter) painter)

(define (flipped-pairs painter)
  (let ((upside-down (beside painter (flip-vert painter))))
    (below upside-down upside-down)))

(define (right-split painter n)
  (if (= n 0)
      (identity painter)
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      (identity painter)
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      (identity painter)
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (top-right (corner-split painter (- n 1)))
              (bottom-left painter)
              (bottom-right (below right right)))
          (below (beside top-left top-right)
                 (beside bottom-left bottom-right))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))