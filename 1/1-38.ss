
(define (cont-frac n d k)
  (define (iter i result)
    (let ((cur (/ (n i)
                  (+ (d i) result))))
         (if (= 1 i)
             cur
             (iter (- i 1) cur))))
  (iter k 0))

(define (getE)
  (define (getD i)
    (let ((ip (+ i 1)))
         (if (= 0 (remainder ip 3))
             (* 2 (/ ip 3.0))
             1.0)))
  (define (getEm2)
    (cont-frac (lambda (i) 1.0)
               getD
               1000))
  (+ 2 (getEm2)))

(display (getE))


(exit)
