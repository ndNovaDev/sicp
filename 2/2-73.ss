(define (dn x)
  (display x)
  (newline))

(define nil '())

(define true #t)

(define false #f)

; **********************

; a:不懂
; b:只写sum

(define (sum exp var)
  (make-sum (deriv (addend exp) var)
            (deriv (augend exp) var)))

(put 'deriv '(+) sum)

; c:不想做,几乎和b一样
; d: 只需修改put get逻辑


(exit)
