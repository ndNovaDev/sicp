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

(define pa (segments->painter (list (make-segment (make-vect 0.0 0.0) (make-vect 0.2 0.0))
                                    (make-segment (make-vect 0.2 0.0) (make-vect 0.2 0.2))
                                    (make-segment (make-vect 0.2 0.2) (make-vect 0.0 0.2))
                                    (make-segment (make-vect 0.0 0.2) (make-vect 0.0 0.0))
                                    (make-segment (make-vect 0.0 0.0) (make-vect 1.0 0.0))
                                    (make-segment (make-vect 1.0 0.0) (make-vect 1.0 1.0))
                                    (make-segment (make-vect 1.0 1.0) (make-vect 0.0 1.0))
                                    (make-segment (make-vect 0.0 1.0) (make-vect 0.0 0.0))
                                    )))

(define base-frame (make-frame (make-vect 0 0)
                (make-vect 499 0)
                (make-vect 0 499)))

; (pa base-frame)


; ***************************************


(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
          (make-frame new-origin
                      (sub-vect (m corner1) new-origin)
                      (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.36 0.65)))
; ((flip-vert pa) base-frame)
; ((shrink-to-upper-right pa) base-frame)
; ((rotate90 pa) base-frame)
; ((squash-inwards pa) base-frame)

; *****************************************************

; (define (flip-horiz painter)
;   (transform-painter painter
;                      (make-vect 1.0 0.0)
;                      (make-vect 0.0 0.0)
;                      (make-vect 1.0 1.0)))

; ; ((flip-horiz pa) base-frame)

; (define (rotate180 painter)
;   (transform-painter painter
;                      (make-vect 1.0 1.0)
;                      (make-vect 0.0 1.0)
;                      (make-vect 1.0 0.0)))

; ; ((rotate180 pa) base-frame)

; (define (rotate90+ painter)
;   (transform-painter painter
;                      (make-vect 1.0 0.0)
;                      (make-vect 1.0 1.0)
;                      (make-vect 0.0 0.0)))
(define (rotate-90 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

; ((rotate90+ pa) base-frame)

; ******************************************************

; (define (below p1 p2)
;   (let ((split-point (make-vect 0.0 0.5)))
;        (let ((pb (transform-painter p1
;                                     split-point
;                                     (make-vect 1.0 0.5)
;                                     (make-vect 0.0 1.0)))
;              (pt (transform-painter p2
;                                     (make-vect 0.0 0.0)
;                                     (make-vect 1.0 0.0)
;                                     split-point)))
;             (lambda (frame)
;               (pt frame)
;               (pb frame)))))
(define (beside p1 p2)
  (let ((split-point (make-vect 0.5 0.0)))
       (let ((pb (transform-painter p1
                                    (make-vect 0.0 0.0)
                                    split-point
                                    (make-vect 0.0 1.0)))
             (pt (transform-painter p2
                                    split-point
                                    (make-vect 1.0 0.0)
                                    (make-vect 0.5 1.0))))
            (lambda (frame)
              (pt frame)
              (pb frame)))))

(define (below p1 p2)
  (let ((rp1 (rotate-90 p1))
        (rp2 (rotate-90 p2)))
       (let ((rb (beside rp1 rp2)))
            (rotate90 rb))))


((below pa pa) base-frame)
; (pa base-frame)
