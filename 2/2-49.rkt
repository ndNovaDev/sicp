#lang racket/gui

(require graphics/graphics)
(open-graphics)
(define vp (open-viewport "A Picture Language" 500 500))

(define draw (draw-viewport vp))
(define (clear) ((clear-viewport vp)))
(define line (draw-line vp))

; ********************
(define (dn x)
  (display x)
  (newline))
(define nil '())
(define (for-each p items)
  (cond ((not (null? items)) (p (car items)) (for-each p (cdr items)))))
; ***********************
(define (make-vect x y)
  (cons x y))
(define (xcor-vect v)
  (car v))
(define (ycor-vect v)
  (cdr v))
(define (op-vect op v1 v2)
  (make-vect (op (xcor-vect v1) (xcor-vect v2))
        (op (ycor-vect v1) (ycor-vect v2))))

(define (add-vect v1 v2)
  (op-vect + v1 v2))
(define (sub-vect v1 v2)
  (op-vect - v1 v2))
(define (scale-vect x v)
  (make-vect (* x (xcor-vect v))
        (* x (ycor-vect v))))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (cadr frame))
(define (edge2-frame frame)
  (cdr (cdr frame)))

(define (make-segment start end)
  (cons start end))
(define (start-segment s)
  (car s))
(define (end-segment s)
  (cdr s))



;(line (make-posn 2 6) (make-posn 80 666))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
      (origin-frame frame)
      (add-vect (scale-vect (xcor-vect v)
                            (edge1-frame frame))
                (scale-vect (ycor-vect v)
                            (edge2-frame frame))))))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
      (lambda (segment)
        (line-draw
          ((frame-coord-map frame) (start-segment segment))
          ((frame-coord-map frame) (end-segment segment))))
      segment-list)))

(define (line-draw x y)
  (dn x)
  (dn y)
  (newline)
  (line (make-posn (xcor-vect x) (ycor-vect x))
        (make-posn (xcor-vect y) (ycor-vect y))))

(define pa (segments->painter (list (make-segment (make-vect 0.0 0.0) (make-vect 1.0 0.0))
                                    (make-segment (make-vect 1.0 0.0) (make-vect 1.0 1.0))
                                    (make-segment (make-vect 1.0 1.0) (make-vect 0.0 1.0))
                                    (make-segment (make-vect 0.0 1.0) (make-vect 0.0 0.0)))))

(define pb (segments->painter (list (make-segment (make-vect 0.0 0.0) (make-vect 1.0 1.0))
                                    (make-segment (make-vect 1.0 0.0) (make-vect 0.0 1.0)))))

(define pc (segments->painter (list (make-segment (make-vect 0.5 1.0) (make-vect 1.0 0.5))
                                    (make-segment (make-vect 1.0 0.5) (make-vect 0.5 0.0))
                                    (make-segment (make-vect 0.5 0.0) (make-vect 0.0 0.5))
                                    (make-segment (make-vect 0.0 0.5) (make-vect 0.5 1.0)))))

(pa (make-frame (make-vect 0 0)
                (make-vect 100 0)
                (make-vect 0 100)))
(pb (make-frame (make-vect 0 0)
                (make-vect 100 0)
                (make-vect 0 100)))
(pc (make-frame (make-vect 0 0)
                (make-vect 100 0)
                (make-vect 0 100)))


