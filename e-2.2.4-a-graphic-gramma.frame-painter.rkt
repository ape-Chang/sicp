#lang racket

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect (origin-frame frame)
              (add-vect (scale-vect (xcor-vect v)
                                    (edge1-frame frame))
                        (scale-vect (ycor-vect v)
                                    (edge2-frame frame))))))
;;
(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))
(define (add-vect v u)
  (make-vect (+ (xcor-vect v)
                (xcor-vect u))
             (+ (ycor-vect v)
                (ycor-vect u))))
(define (sub-vect v u)
  (make-vect (- (xcor-vect v)
                (xcor-vect u))
             (- (ycor-vect v)
                (ycor-vect u))))
(define (scale-vect k v)
  (make-vect (* k (xcor-vect v))
             (* k (ycor-vect v))))
;;
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (car (cdr frame)))
(define (edge2-frame frame)
  (car (cdr (cdr frame))))
;;
(define (foreach proc list)
  (define (iter last-result rest)
    (if (null? rest)
        last-result
        (iter (proc (car rest)) (cdr rest))))
  (iter '() list))
;;
(define (draw-line x y) '())
;;
(define (make-segment start end) (cons start end))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))
;;
(define (segments-painter segment-list)
  (lambda (frame)
    (foreach (lambda (segment)
               (let ((frame-translate (frame-coord-map frame)))
                 (draw-line (frame-translate (start-segment segment))
                            (frame-translate (end-segment segment)))))
             segment-list)))
;;
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame new-origin
                             (sub-vect (m corner1) new-origin)
                             (sub-vect (m corner2) new-origin)))))))
;;
(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))
(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))
(define (rotate90 painter)
  (transform-painter painter
                     (make-vector 1.0 0.0)
                     (make-vector 1.0 1.0)
                     (make-vector 0.0 0.0)))
(define (rotate180 painter)
  (transform-painter painter
                     (make-vector 1.0 1.0)
                     (make-vector 1.0 0.0)
                     (make-vector 0.0 1.0)))
(define (rotate270 painter)
  (transform-painter painter
                     (make-vector 0.0 1.0)
                     (make-vector 1.0 1.0)
                     (make-vector 0.0 0.0)))
(define (squash-inwards painter)
  (transform-painter painter
                     (make-vector 0.0 0.0)
                     (make-vector 0.65 0.35)
                     (make-vector 0.35 0.65)))
(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left (transform-painter painter1
                                         (make-vect 0.0 0.0)
                                         split-point
                                         (make-vect 0.0 1.0)))
          (paint-right (transform-painter painter2
                                          split-point
                                          (make-vect 1.0 0.0)
                                          (make-vect 0.5 1.0))))
      (lambda (frame) (paint-left frame) (paint-right frame)))))