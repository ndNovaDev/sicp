(define (dn x)
  (display x)
  (newline))

(define nil '())

(define true #t)

(define false #f)

; **********************

; equ?

(put 'equ? '(scheme-number scheme-number)
  (lambda (x y) (tag (= x y))))

; =zero?

(put '=zero? '(scheme-number)
  (lambda (x) (tag (= x 0))))

(exit)
