(define (double x)
  (* x 2))
(define (halve x)
  (/ x 2))
(define (even n)
  (= 0 (remainder n 2)))

(define (fast* a b)
  (cond ((= b 1) a)
        ((even b) (fast* (double a) (halve b)))
        (else (+ a (fast* a (- b 1))))))

(display (fast* 3 9))

(exit)
