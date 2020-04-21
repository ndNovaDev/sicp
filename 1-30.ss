(define (cube n) (* n n n))
(define (inc n) (+ n 1))

; (define (sum term a next b)
;   (if (> a b)
;       0
;       (+ (term a)
;          (sum term (next a) next b))))

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))


(define (sum-cubes a b)
  (sum cube a inc b))

(display (sum-cubes 1 10))




(exit)