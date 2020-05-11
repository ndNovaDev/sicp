#lang racket

 ( require ( planet "sicp.ss" ( "soegaard" "sicp.plt" 2 1)))

(define wave einstein)

;*****************************

(define (split op1 op2)
  (define (iter painter n)
    (if (= 0 n)
        painter
        (let ((next (iter painter (- n 1))))
             (op1 painter
                  (op2 next next)))))
  (lambda (painter n)
    (iter painter n)))

(define right-split (split beside below))
(define up-split (split below beside))

(paint (right-split wave 6))
(paint (up-split wave 6))