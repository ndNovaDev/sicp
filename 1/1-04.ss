(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; 当b>0时，返回+操作符
; 当b<=0时，返回-操作符

(display (a-plus-abs-b 10 9))
(display (a-plus-abs-b 10 -9))

(exit)
