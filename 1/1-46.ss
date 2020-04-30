
(define (ii t i)
  (define (iter x)
    (if (t x)
        x
        (iter (i x))))
  (lambda (x)
    (iter x)))


(define (t x)
  (if (= 4 x)
      #t
      #f))
(define (i x)
  (- x 1))


(display ((ii t i) 20))
(exit)
