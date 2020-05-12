
(define (dn x)
  (display x)
  (newline))
(define nil '())

; ***********************

(define (make-segment start end)
  (cons start end))
(define (start-segment s)
  (car s))
(define (end-segment s)
  (cdr s))

(exit)
