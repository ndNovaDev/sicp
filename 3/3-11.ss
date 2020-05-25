(define (dn x)
  (display x)
  (newline))

; **********************

(define acc (make-account 50))
((acc 'deposit) 40)
; 90
((add 'withdraw) 60)
; 30

; acc的局部状态保存在哪？
; 保存在自己的E

; (define acc2 (make-account 100))
; 环境结构中，哪些部分被acc和acc2共享？
; make-account的体？

(exit)
