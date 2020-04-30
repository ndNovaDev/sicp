
(define (square n)
  (* n n))
(define (compose f g)
  (lambda (x)
    (f (g x))))
(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

(define dx 0.00001)
; (define (smooth f)
;   (lambda (x)
;     (/ (+ (f (- x dx))
;           (f x)
;           (f (+ x dx)))))
;        3)))

(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3)))

(define (smoothN n)
  (repeated smooth n))

; (display ((smooth square) 5))

(display (((smoothN 10) square) 5))

(exit)
