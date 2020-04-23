

(define (fp f fg)
  (define tolerance 0.0001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
         (if (close-enough? guess next)
             next
            (try next))))
  (try fg))

(display (fp (lambda (x) (+ 1 (/ 1 x))) 1.0))

(exit)