
; search-for-primes
(define (square n)
  (* n n))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))


(define (prime? n)
  (= n (smallest-divisor n)))

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
        ((prime? current) (display current)(newline)(find3PrimeGTNI (+ foundCount 1) (+ current 2)))
        (else (find3PrimeGTNI foundCount (+ current 2)))))

(time (find3PrimeGTN 1000))
; 1009
; 1013
; 1019
; Done!(time (find3PrimeGTN 1000))
;     no collections
;     0.000033391s elapsed cpu time
;     0.000031000s elapsed real time
;     768 bytes allocated
(time (find3PrimeGTN 10000))
; 10007
; 10009
; 10037
; Done!(time (find3PrimeGTN 10000))
;     no collections
;     0.000020607s elapsed cpu time
;     0.000021000s elapsed real time
;     768 bytes allocated
(time (find3PrimeGTN 100000))
; 100003
; 100019
; 100043
; Done!(time (find3PrimeGTN 100000))
;     no collections
;     0.000039122s elapsed cpu time
;     0.000039000s elapsed real time
;     768 bytes allocated
(time (find3PrimeGTN 1000000))
; 1000003
; 1000033
; 1000037
; Done!(time (find3PrimeGTN 1000000))
;     no collections
;     0.000082113s elapsed cpu time
;     0.000082000s elapsed real time
;     768 bytes allocated

(exit)