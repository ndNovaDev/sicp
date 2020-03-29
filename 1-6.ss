
(define (goodEnough? g x)
  (< (abs (- (* g g) x))
     0.001))

(define (improve g x)
  (/ (+ g
        (/ x g))
     2))

(define (sqI g x)
  (if (goodEnough? g x)
      g
      (sqI (improve g x) x)))

(define (sq x)
  (sqI 1.0 x))


; (display (sq 137))
; +++++++++++++++++++++++++++++++++++++++++++++++++++++++

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

; (display (new-if (= 2 3) 0 5))
; (display (new-if (= 1 1) 0 5))


(define (sqrt-iter guess x)
  (new-if (goodEnough? guess x)
          guess
          (sqrt-iter (improve guess x)
                      x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(display (sqrt 4))
; 程序会陷入死循环

; 因为会对所有参数求值，sqrt-iter会不停调用sqrt-iter


(exit)