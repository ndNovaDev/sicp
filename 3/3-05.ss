(define (dn x)
  (display x)
  (newline))

; **********************

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

; **********************

(define (random-in-range low high)
  (let ((range (- high low)))
       (+ low (random range))))

; *******************


(define (estimate-integral x1 x2 y1 y2 trials)
  (define (exp)
    (let ((x (/ (+ x1 x2) 2))
          (y (/ (+ y1 y2) 2))
          (random-x (random-in-range x1 x2))
          (random-y (random-in-range y1 y2))
          (r (/ (- x2 x1) 2)))
         (> (* r r)
            (+ (* (- random-x x) (- random-x x))
               (* (- random-y y) (- random-y y))))))
  (let ((ratio (monte-carlo 10000 exp))
        (w (- x2 x1))
        (h (- y2 y1))
        (r (/ (- x2 x1) 2)))
       (let ((s (* ratio w h)))
            (/ s (* r r) 1.0))))

(dn (estimate-integral 1 1000 1 1000 10000))

(exit)
