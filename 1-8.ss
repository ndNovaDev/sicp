
(define (goodEnough? lg g)
  (< (abs (- lg g))
     0.001))

(define (improve y x)
  (/ (+ (/ x
           (* y y))
        (* 2 y))
     3))

(define (cubeRootI lg g x)
  (if (goodEnough? lg g)
      g
      (cubeRootI g (improve g x) x)))

(define (cubeRoot x)
  (cubeRootI 0 1.0 x))


(display (cubeRoot (* 21 21 21)))
; +++++++++++++++++++++++++++++++++++++++++++++++++++++++


(exit)