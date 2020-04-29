
(define (expt base n)
  (if (= n 0)
      1
      (* base (expt base (- n 1)))))

(define (lg n)
  (cond ((> (/ n 2) 1) (+ 1 (lg (/ n 2))))
        ((< (/ n 2) 1) 0)
        (else 1)))

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

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

(define (averrage-damp f)
  (lambda (x) (/ (+ x (f x)) 2)))

; **********************************************

(define (nthRoot base n)
  (let ((f (lambda (y) (/ base (expt y (- n 1)))))
        (dampFn (repeated averrage-damp (lg n))))
       (let ((df (dampFn f)))
            (fp df 1.0))))

(display (nthRoot 65536 4))

(exit)
