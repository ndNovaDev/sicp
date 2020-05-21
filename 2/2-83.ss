(define (dn x)
  (display x)
  (newline))

(define nil '())

(define true #t)

(define false #f)

; **********************

 (define (raise x) (apply-generic 'raise x)) 
  
 ;; add into scheme-number package 
 (put 'raise 'integer  
          (lambda (x) (make-rational x 1))) 
  
 ;; add into rational package 
 (put 'raise 'rational 
          (lambda (x) (make-real (/ (numer x) (denom x))))) 
  
 ;; add into real package 
 (put 'raise 'real 
          (lambda (x) (make-from-real-imag x 0))) 

; http://community.schemewiki.org/?sicp-ex-2.83

(exit)
