(define (dn x)
  (display x)
  (newline))

; **********************

(define (get-f)
  (define ret0 #f)
  (lambda (n)
    (begin (if (= 0 n) (set! ret0 #t))
           (if ret0 0 n))))
(define f (get-f))

(dn (+ (f 1) (f 0)))
; (dn (+ (f 0) (f 1)))

(exit)
