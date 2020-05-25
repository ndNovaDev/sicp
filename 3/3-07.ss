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
  (define (validate pwd)
    (eq? pwd password))
  (define (dispatch pwd m)
    (if (validate pwd)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              ((eq? m 'validate) validate)
              (else "sb"))
        "sba"))
  dispatch)


(define (make-joint acc old-pwd pwd)
  (if ((acc old-pwd 'validate) old-pwd)
      (lambda (password m)
        (if (eq? password pwd)
            (acc old-pwd m)
            (error "w" "w")))
      (error "joint canceled" "joint canceled")))

(define peter-acc (make-account 100 'abc))
(define paul-acc
  (make-joint peter-acc 'abc 'def))

(dn ((paul-acc 'def 'deposit) 1000))






(exit)
