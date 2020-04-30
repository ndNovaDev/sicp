(define (next n) (+ n 1))
(define (noneTerm n) n)
(define (isEven n)
  (= 0 (remainder n 2)))

(define (accumulate filter combiner null-value term a next b)
  (define (iter a result)
    (cond ((> a b) result)
          ((filter a) (iter (next a) (combiner (term a) result)))
          (else (iter (next a) result))))
  (iter a null-value))

; 偷懒，只计算了偶数相加，没有算素数和、互素乘积
(define (evenSum a b)
  (accumulate isEven + 0 noneTerm a next b))

(display (evenSum 1 8))


(exit)