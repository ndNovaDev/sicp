(define (dn x)
  (display x)
  (newline))
(define (square n)
  (* n n))


(define (tree-map proc tree)
  (cond ((null? tree) '())
        ((pair? tree) (cons (tree-map proc (car tree))
                            (tree-map proc (cdr tree))))
        (else (proc tree))))
(define (square-tree tree)
  (tree-map square tree))

(dn (square-tree
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7))))

(exit)
