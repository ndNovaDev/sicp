(define (dn x)
  (display x)
  (newline))

(define nil '())

(define true #t)

(define false #f)

; **********************

(define (make-accumulator value)
  (lambda (n)
    (begin (set! value (+ value n))
           value)))

(define A (make-accumulator 5))
(dn (A 10))
(dn (A 10))

(exit)
