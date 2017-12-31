#lang racket
(require compatibility/mlist)

(define (make-table) (mlist '*table*))

(define (assoc key records)
  (if (null? records)
      #f
      (let ((first (mcar records))
            (rest (mcdr records)))
        (if (equal? key (mcar first))
            first
            (assoc key rest)))))

(define (lookup key table)
  (let ((records (mcdr table)))
    (let ((record (assoc key records)))
      (if record (mcdr record) #f))))

(define (insert! key value table)
  (let ((records (mcdr table)))
    (let ((record (assoc key records)))
      (if record
          (set-mcdr! record value)
          (set-mcdr! table (mcons (mcons key value)
                                  records))))))
