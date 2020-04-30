(define (f n)
  (if (< n 3)
      n
      (+ (* 1 (f (- n 1)))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))

; (display (f 5))

; 上面是递归实现
; 下面是迭代实现

; f(0) = 0
; f(1) = 1
; f(2) = 2
; f(3) = f(2) + 2f(1) + 3f(0)
; f(4) = f(3) + 2f(2) + 3f(1)
; f(5) = f(4) + 2f(3) + 3f(2)
; f(6) = f(5) + 2f(4) + 3f(3)

(define (ff-i x y z i n)
  (if (= i n)
      z
      (ff-i y
            z
            (+ z
               (* 2 y)
               (* 3 x))
            (+ i 1)
            n)))

(define (ff n)
  (if (< n 3)
      n
      (ff-i 0 1 2 2 n)))

; (display (ff 5))

(exit)