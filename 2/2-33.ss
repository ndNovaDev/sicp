(define (dn x)
  (display x)
  (newline))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; ***********************************
(define items1 (list 1 2 3 4 5))
(define items2 (list 6 7 8 9 0))

(define (map p sequence)
  (accumulate (lambda (x y)
                (cons (p x) y))
              '()
              sequence))
(dn (map (lambda (x) (* x x)) items1))


(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
(dn (append items1 items2))


(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))
(dn (length items1))

(exit)
