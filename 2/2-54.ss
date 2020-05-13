(define (dn x)
  (display x)
  (newline))
(define nil '())

; *****************

(define (equal? a b)
  (if (and (pair? a) (pair? b))
      (and (equal? (cdr a) (cdr b))
           (eq? (car a) (car b)))
      (eq? a b)))

(dn (equal? 'sb 'sb))
(dn (equal? 'sb 's))
(dn (equal? (list 'a 'b 'c) (list 'a 'b 'c)))
(dn (equal? (list 'a) (list 'a 'b 'c)))

(exit)
