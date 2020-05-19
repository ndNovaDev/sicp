(define (dn x)
  (display x)
  (newline))

(define nil '())

(define true #t)

(define false #f)

; **********************

(define (make-from-mag-ang mag ang)
  (lambda (op)
    (if (eq? op 'mag)
        mag
        ang)))

; 简单写一下

(exit)
