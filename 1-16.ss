(define (square n)
  (* n n))

(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))


; (display (fast-expt 2 3))

(define (fei-i b n a)
  (cond ((= n 0) a)
        ((even? n) (fei-i (square b) (/ n 2) a))
        (else (fei-i b (- n 1) (* b a)))))

(define (fei b n)
  (fei-i b n 1))

(display (fei 2 5))
(exit)