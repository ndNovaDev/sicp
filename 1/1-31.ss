
(define (next n) (+ n 1))
(define (noneTerm n) n)

; (define (product term a next b)
;   (if (> a b)
;       1
;       (* (term a) (product term (next a) next b))))


(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorail n)
  (product noneTerm 1 next n))

(display (factorail 4))

(newline)

(define (isEven n)
  (= 0 (remainder n 2)))
(define (getNumerator n)
  (if (isEven n)
      (+ n 2)
      (+ n 1)))
(define (getDenominator n)
  (if (isEven n)
      (+ n 1)
      (+ n 2)))
(define (piTerm n)
  (/ (getNumerator n) (getDenominator n)))
(define (pi)
  (* 4.0 (product piTerm 1 next 1000)))

(display (pi))

(exit)