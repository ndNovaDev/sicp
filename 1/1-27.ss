
; 561
; 1105
; 1729
; 2465
; 2821
; 6601

(define (test n)
  (define (square n)
    (* n n))
  (define (even? n)
    (= 0 (remainder n 2)))
  (define (expmod base exp m)
    (cond ((= exp 0) 1)
          ((even? exp)
          (remainder (square (expmod base (/ exp 2) m))
                    m))
          (else
            (remainder (* base (expmod base (- exp 1) m))
                      m))))
  (define (testI n i)
    (cond ((= i n) #t)
          ((= i (expmod i n n)) (testI n (+ i 1)))
          (else #f)))
  (testI n 0))

(display (test 561))
(newline)
(display (test 1105))
(newline)
(display (test 1729))
(newline)
(display (test 2465))
(newline)
(display (test 2821))
(newline)
(display (test 6601))
(newline)
(display (test 6602))
(newline)

(exit)