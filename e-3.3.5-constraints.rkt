#lang racket

(define (memq item list)
  (cond ((null? list) #f)
        ((eq? item (car list)) #t)
        (else (memq item (cdr list)))))
;;
(define (make-connector)
  (let ((value #f)
        (informant #f)
        (constraints '()))

    (define (set-value! new-value setter)
      (set! value new-value)
      (set! informant setter))

    (define (forget! retractor)
      (if (eq? retractor informant)
          (set! value #f)
          'ignore))

    (define (has-value?) (if informant #t #f))

    (define (connect new-constraint)
      (if (memq new-constraint constraints)
          'ignore
          (set! constraints (cons new-constraint constraints)))
      'done)

    (define (dispatch m)
      (cond ((eq? m 'has-value?) has-value?)
            ((eq? m 'value) value)
            ((eq? m 'set-value!) set-value!)
            ((eq? m 'forget!) forget!)
            ((eq? m 'connect) connect)
            (else (error "Unknown operation -- CONNECTOR" m))))

    dispatch))
(define (has-value? connector) ((connector 'has-value?)))
(define (get-value connector) (connector 'value))
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor)
  ((connector 'forget!) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))