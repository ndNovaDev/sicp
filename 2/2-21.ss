(define (square x)
  (* x x))

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items))
            (square-list (cdr items)))))

(define (square-list-2 items)
  (map square items))





(define items (list 1 2 3 4))

(display (square-list items))

(newline)

(display (square-list-2 items))


(exit)
