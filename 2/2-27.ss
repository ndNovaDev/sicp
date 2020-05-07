
(define (deep-reverse items)
  (if (pair? items)
      (list (deep-reverse (car (cdr items)))
            (deep-reverse (car items)))
      items))

(define x (list (list 1 2) (list 3 4)))
(display x)
(newline)
(display (deep-reverse x))
; ((4 3) (2 1))

(exit)
