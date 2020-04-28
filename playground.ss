
(define (fp f fg)
  (define tolerance 0.0001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (newline)
    (let ((next (f guess)))
         (if (close-enough? guess next)
             next
            (try next))))
  (try fg))

(define dx 0.00001)
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (newton-method g guess)
  (fp (newton-transform g) guess))

(define (sqrt x)
  (newton-method (lambda (y) (- (* y y) x)) 1.0))

(display (sqrt 16))


(exit)
