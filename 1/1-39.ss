
(define (cont-frac n d k)
  (define (iter i result)
    (let ((cur (/ (n i)
                  (+ (d i) result))))
         (if (= 1 i)
             cur
             (iter (- i 1) cur))))
  (iter k 0))

(define (tan-cf x k)
  (define (getN i)
    (if (= 1 i)
        (* 1.0 x)
        (- (* x x 1.0))))
  (define (getD i)
    (- (* 2.0 i) 1))
  (cont-frac getN getD k))

(display (tan-cf 25 100))


(exit)
