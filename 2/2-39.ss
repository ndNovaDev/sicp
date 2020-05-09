(define (dn x)
  (display x)
  (newline))
(define nil '())
(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))
(define (fold-left op initial sequence)
  (if (null? sequence)
      initial
      (fold-left op
                  (op initial (car sequence))
                  (cdr sequence))))
; *********************

(define (reverse-r sequence)
  (fold-right (lambda (x y)
                (append y (list x))) nil sequence))
(define (reverse-l sequence)
  (fold-left (lambda (x y)
                (cons y x)) nil sequence))

(dn (reverse-r (list 1 2 3 4 5)))
(dn (reverse-l (list 1 2 3 4 5)))

(exit)
