(define (dn x)
  (display x)
  (newline))
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map (lambda (x)
                                        (car x))
                                     seqs))
            (accumulate-n op init (map (lambda (x)
                                        (cdr x))
                                       seqs)))))

(define matrix (list (list 1 2 3 4) (list 5 6 7 8) (list 9 10 11 12))) 

(define (dot-product v1 v2) 
  (accumulate + 0 (map * v1 v2))) 
(dn (dot-product (list 1 2 3) (list 4 5 6)) )

; ***********************

(define (matrix-*-vector m v)
  (map (lambda (r) (dot-product r v)) m))
(dn (matrix-*-vector matrix (list 2 3 4 5)))

; 
(define nil '()) 
(define (transpose m) 
   (accumulate-n cons nil m)) 
(dn (transpose matrix))

(define (matrix-*-matrix m n) 
  (let ((n-cols (transpose n))) 
    (map (lambda (m-row) (matrix-*-vector n-cols m-row)) 
        m))) 
(dn (matrix-*-matrix matrix (list (list 1 2) (list 1 2) (list 1 2) (list 1 2))) )

(exit)
