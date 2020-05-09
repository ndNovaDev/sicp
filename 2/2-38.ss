(define (dn x)
  (display x)
  (newline))
(define nil '())

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))
; (define (fold-left op initial sequence)
;   (if (null? sequence)
;       initial
;       (fold-left op
;                   (op initial (car sequence))
;                   (cdr sequence))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

; ****************************

(dn (fold-right / 1 (list 1 2 3)))
; 3/2
(dn (fold-left / 1 (list 1 2 3)))
; 1/6
(dn (fold-right list nil (list 1 2 3)))
; (1 (2 (3 ())))
(dn (fold-left list nil (list 1 2 3)))
; (((() 1) 2) 3)

; 因为 fold-left 和 fold-right 生成的计算序列不同，要让它们的计算产生同样的结果，一个办法就是要求 op 参数，也即是传入的操作函数必须符合结合律（monoid）。

; 比如说， \ 和 list 函数都不符合结合律，所以将它们应用到 fold-left 和 fold-right 会产生不同的计算结果。

; 另一方面，像 + 、 * 、 or 和 and 那样的函数，就是符合结合律的函数，使用这些函数可以让 fold-left 和 fold-right 计算出同样的结果：
(exit)
