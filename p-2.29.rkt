#lang racket
;; 2-29-a
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

(define (is-mobile? structure)
  (pair? structure))
;; 2-29-b
(define (total-weight mobile)
  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
    
    (+ (let ((structure (branch-structure left)))
         (if (is-mobile? structure)
             (total-weight structure)
             structure))

       (let ((structure (branch-structure right)))
         (if (is-mobile? structure)
             (total-weight structure)
             structure)))))

(define (is-balance? structure)
  (if (is-mobile? structure)
      (let ((left (left-branch structure))
            (right (right-branch structure)))
        (and (is-balance? left)
             (is-balance? right)
             (= (* (branch-length left)
                   (total-weight left))
                (* (branch-length right)
                   (total-weight right)))))
      #t))