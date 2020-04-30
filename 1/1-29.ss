
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (cube n) (* n n n))
(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))

(display (sum-cubes 1 10))
(newline)

(define (identity x) x)
(define (sum-integers a b)
  (sum identity a inc b))

(display (sum-integers 1 10))
(newline)

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))
(display (* 8 (pi-sum 1 100)))
(newline)

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* dx (sum f (+ a (/ dx 2.0)) add-dx b)))
(display (integral cube 0 1 0.01))
(newline)

(define (odd? n)
  (= 1 (remainder n 2)))



(define (simpson f a b n)
  (define h (/ (- b a)
             n))
  (define (y k)
    (f (+ a (* k h))))
  (define (factor k)
    (cond ((or (= k 0) (= k n))
            1)
          ((odd? k)
            4)
          (else
            2)))
  (define (term k)
    (* (factor k)
       (y k)))
  (* (/ h 3)
     (sum term a inc n)))

(display (simpson cube 0 1 100))

(exit)
