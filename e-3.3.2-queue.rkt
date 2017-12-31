#lang racket

(require compatibility/mlist)

;; interface
(define (make-queue) (mcons '() '()))
(define (empty-queue? queue) (null? (front-ptr queue)))
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (mcar (front-ptr queue))))
(define (insert-queue! queue item)
  (let ((new-pair (mcons item '())))
    (if (empty-queue? queue)
        (begin
          (set-front-ptr! queue new-pair)
          (set-rear-ptr! queue new-pair)
          queue)
        (begin
          (set-mcdr! (rear-ptr queue) new-pair)
          (set-rear-ptr! queue new-pair)
          queue))))
(define (delete-queue! queue)
  (if (empty-queue? queue)
      (error "DELETE! called with an empty queue" queue)
      (set-front-ptr! queue (mcdr (front-ptr queue)))))
(define (print-queue queue)
  (mlist->list (front-ptr queue)))
;; 
(define (front-ptr queue) (mcar queue))
(define (rear-ptr queue) (mcdr queue))
(define (set-front-ptr! queue item) (set-mcar! queue item))
(define (set-rear-ptr! queue item) (set-mcdr! queue item))
;;
(define q1 (make-queue))
(print-queue (insert-queue! q1 'a))
(print-queue (insert-queue! q1 'b))