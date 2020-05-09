
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))
(define (make-point x y)
  (cons x y))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment a b)
  (cons a b))
(define (start-segment s)
  (car s))
(define (end-segment s)
  (cdr s))

(define (midpoint-segment seg)
  (let ((startX (x-point (start-segment seg)))
        (endX (x-point (end-segment seg)))
        (startY (y-point (start-segment seg)))
        (endY (y-point (end-segment seg))))
       (let ((avgX (/ (+ startX endX) 2.0))
             (avgY (/ (+ startY endY) 2.0)))
            (make-point avgX avgY))))


; *********************************************
(define start (make-point 1 3))
(define end (make-point 4 3))
(define seg (make-segment start end))
(define mid (midpoint-segment seg))

(print-point mid)

(exit)
