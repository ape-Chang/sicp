#lang racket
(require compatibility/mlist)
;; 
(define (make-table)
  (let ((local-table (mlist '*table*)))
    
    (define (assoc key records)
      (if (null? records)
          #f
          (let ((first (mcar records))
                (rest (mcdr records)))
            (if (equal? key (mcar first))
                first
                (assoc key rest)))))
    
    (define (lookup key-1 key-2)
      (let ((tables (mcdr local-table)))
        (let ((table (assoc key-1 tables)))
          (if table
              (let ((records (mcdr table)))
                (let ((record (assoc key-2 records)))
                  (if record
                      (mcdr record)
                      #f)))
              #f))))

    (define (insert! key-1 key-2 value)
      (let ((tables (mcdr local-table)))
        (let ((table (assoc key-1 tables)))
          (if table
              (let ((records (mcdr table)))
                (let ((record (assoc key-2 records)))
                  (if record
                      (set-mcdr! record value)
                      (set-mcdr! table (mcons (mcons key-2 value)
                                              records)))))
              (set-mcdr! local-table (mcons (mlist key-1 (mcons key-2 value))
                                            tables))))))

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc) insert!)
            (else (error "Unknown operation -- TABLE" m))))

    dispatch))
;; double dispatch
(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc))