(define (dn x)
  (display x)
  (newline))

(define nil '())

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

(define v1 (make-vect 10 20))
(define v2 (make-vect 4 10))
(dn v1)
(dn v2)
(dn (sub-vect v1 v2))
(dn (scale-vect 2 v1))

(exit)
