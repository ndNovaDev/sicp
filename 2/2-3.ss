
(define (makeRect w h)
  (cons w h))
(define (getW rect)
  (car rect))
(define (getH rect)
  (cdr rect))
; **************************

(define (getArea rect)
  (* (getW rect) (getH rect)))
(define (getPerimeter rect)
  (* 2 (+ (getW rect) (getH rect))))

; *****************************
(define rect (makeRect 2 3))
(display (getArea rect))
(newline)
(display (getPerimeter rect))


(exit)
