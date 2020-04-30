

(define (fp f fg)
  (define tolerance 0.0001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
         (if (close-enough? guess next)
             next
            (try next))))
  (try fg))


; (display (fp (lambda (x) (/ (log 1000) (log x))) 3)) ;28
; (display (fp (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2)) 3)) ;7
(exit)