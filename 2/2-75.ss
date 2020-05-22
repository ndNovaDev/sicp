(define (dn x)
  (display x)
  (newline))

(define nil '())

(define true #t)

(define false #f)

; **********************


(define (make-from-mag-ang mag ang)
  (lambda (op)
    (cond ((eq? op 'real-part)
           (* mag (cos ang))
          ((eq? op 'imag-part)
           (* mag (sin ang))
          ((eq? op 'magnitude) mag)
          ((eq? op 'angle) ang)
          (else (error "Unknown op -- MAKE-FROM-MAG-ANG" op)))))

(exit)
