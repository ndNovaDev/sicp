(define (double x)
  (* x 2))
(define (halve x)
  (/ x 2))
(define (even n)
  (= 0 (remainder n 2)))

(define (fast*i a b x)
  (cond ((= b 0) x)
        ((even b) (fast*i (double a) (halve b) x))
        (else (fast*i a (- b 1) (+ x a)))))

(define (fast* a b)
  (fast*i a b 0))

(display (fast* 9 6))

(exit)
