(define (dn x)
  (display x)
  (newline))

(define true #t)

(define false #f)

; **********************

; (define balance 100)

; (define (withdraw amount)
;   (if (>= balance amount)
;       (begin (set! balance (- balance amount))
;              balance)
;       "sb"))

(define (new-withdraw)
  (define balance 100)
  (lambda (amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "sb")))

(define withdraw (new-withdraw))

(dn (withdraw 50))
(dn (withdraw 100))
(dn (withdraw 15))



(exit)
