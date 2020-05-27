(define (dn x)
  (display x)
  (newline))

; **********************

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (car queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))
(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "sb" "sb")
      (car (front-ptr queue))))
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
       (cond ((empty-queue? queue)
              (set-front-ptr! queue new-pair)
              (set-rear-ptr! queue new-pair)
              queue)
             (else
              (set-cdr! (rear-ptr queue) new-pair)
              (set-rear-ptr! queue new-pair)
              queue))))
(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "sb" "sb"))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))

; ***********************

(define (dq q)
  (dn (front-ptr q)))

(define q1 (make-queue))
(insert-queue! q1 'a)
(dn q1)
(dq q1)
(insert-queue! q1 'b)
(dn q1)
(dq q1)
(delete-queue! q1)
(dn q1)
(dq q1)

(exit)
