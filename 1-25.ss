
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

; Alyssa 的 expmod 函数在理论上是没有错的，但是在实际中却运行得不好。

; 因为费马检查在对一个非常大的数进行素数检测的时候，可能需要计算一个很大的乘幂，比如说，求十亿的一亿次方，这种非常大的数值计算的速度非常慢，而且很容易因为超出实现的限制而造成溢出。

; 而书本 34 页的 expmod 函数，通过每次对乘幂进行 remainder 操作，从而将乘幂限制在一个很小的范围内（不超过参数 m ），这样可以最大限度地避免溢出，而且计算速度快得多



(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n ) a))
  (try-it (+ 1 (random (- n 1)))))
(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))
; *********************************




; (define (smallest-divisor n)
;   (find-divisor n 2))

; (define (find-divisor n test-divisor)
;   (cond ((> (square test-divisor) n) n)
;         ((divides? test-divisor n) test-divisor)
;         (else (find-divisor n (+ test-divisor 1)))))

; (define (divides? a b)
;   (= (remainder b a) 0))


; (define (prime? n)
;   (= n (smallest-divisor n)))

(define (isOdd? n)
  (= 1 (remainder n 2)))

(define (findSmallestOdd n)
  (if (isOdd? n)
      n
      (+ n 1)))

(define (find3PrimeGTN n)
  (find3PrimeGTNI 0 (findSmallestOdd n)))
  

(define (find3PrimeGTNI foundCount current)
  (cond ((= 3 foundCount) (display "Done!"))
        ((fast-prime? current 3) (display current)(newline)(find3PrimeGTNI (+ foundCount 1) (+ current 2)))
        (else (find3PrimeGTNI foundCount (+ current 2)))))

(time (find3PrimeGTN 1000))
; 1009
; 1013
; 1019
; Done!(time (find3PrimeGTN 1000))
;     no collections
;     0.000058371s elapsed cpu time
;     0.000057000s elapsed real time
;     768 bytes allocated
(time (find3PrimeGTN 10000))
; 10007
; 10009
; 10037
; Done!(time (find3PrimeGTN 10000))
;     no collections
;     0.000047419s elapsed cpu time
;     0.000048000s elapsed real time
;     768 bytes allocated
(time (find3PrimeGTN 100000))
; 100003
; 100019
; 100043
; Done!(time (find3PrimeGTN 100000))
;     no collections
;     0.000057170s elapsed cpu time
;     0.000056000s elapsed real time
;     768 bytes allocated
(time (find3PrimeGTN 1000000))
; 1000003
; 1000033
; 1000037
; Done!(time (find3PrimeGTN 1000000))
;     no collections
;     0.000060065s elapsed cpu time
;     0.000060000s elapsed real time
;     768 bytes allocated

(exit)