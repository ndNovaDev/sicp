(define (dn x)
  (display x)
  (newline))
(define nil '())

(define (memq item x)
  (cond ((null? x) #f)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))
; *****************

(dn (list 'a 'b 'c))
; (a b c)
(dn (list (list 'george)))
; ((george))
(dn (cdr '((x1 x2) (y1 y2))))
; ((y1 y2))
(dn (cadr '((x1 x2) (y1 y2))))
; (y1 y2)
(dn (pair? (car '(a short list))))
; #f
(dn (memq 'red '((red shoes) (blue socks))))
; #f
(dn (memq 'red '(red shoes blue socks)))
; (red shoes blue socks)
(exit)
