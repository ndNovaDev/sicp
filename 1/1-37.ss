
; (define (cont-frac n d k)
;   (define (iter i)
;     (if (= i k)
;         1
;         (/ (n i)
;            (+ (d i) (iter (+ i 1))))))
;   (iter 1))

(define (cont-frac n d k)
  (define (iter i result)
    (let ((cur (/ (n i)
                  (+ (d i) result))))
         (if (= 1 i)
             cur
             (iter (- i 1) cur))))
  (iter k 0))


(display (cont-frac (lambda (i) 1.0)
                    (lambda (i) 1.0)
                    11))

(exit)
