
(define (goodEnough? lg g)
  (< (abs (- lg g))
     0.01))

(define (improve g x)
  (/ (+ g
        (/ x g))
     2))

(define (sqI lg g x)
  (if (goodEnough? lg g)
      g
      (sqI g (improve g x) x)))

(define (sq x)
  (sqI 0 1.0 x))


(display (sq 0.001))
; +++++++++++++++++++++++++++++++++++++++++++++++++++++++


(exit)