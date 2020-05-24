(define (dn x)
  (display x)
  (newline))

(define true #t)

(define false #f)

; **********************

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "sb"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pwd m)
    (if (eq? pwd password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else "sb"))
        "sba"))
  dispatch)

(define acc (make-account 100 'abc))
(dn ((acc 'abc 'withdraw) 40))
(dn ((acc 'def 'deposit) 50))

(exit)
