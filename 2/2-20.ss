
(define (same-add? a b)
  (= (remainder a 2) (remainder b 2)))

(define (same-parity x . l)
  (define (iter lis)
    (if (null? lis)
        '()
        (let ((value (car lis))
              (next (cdr lis)))
             (if (same-add? value x)
                 (cons value (iter next))
                 (iter next)))))
  (iter l))

(display (same-parity 1 2 3 4 5 6 7 8 9 10))
(newline)
(display (same-parity 2 3 4 5 6 7 8 9 10))

(exit)
