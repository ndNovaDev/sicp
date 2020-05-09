(define (dn x)
  (display x)
  (newline))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))
; ***************************

(define (count-leaves t)
  (accumulate (lambda (x y)
                (+ y
                   (if (pair? x)
                       (count-leaves x)
                       1)))
              0
              (map (lambda (item)
                    item)
                   t)))


(define x (cons (list 1 2) (list 3 4)))
(dn (count-leaves (list x x)))

(exit)
