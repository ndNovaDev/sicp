(define (dn x)
  (display x)
  (newline))

; (define (square-tree tree)
;   (cond ((null? tree) '())
;         ((pair? tree) (cons (square-tree (car tree))
;                             (square-tree (cdr tree))))
;         (else (* tree tree))))

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))
(define (square-tree tree)
  (map (lambda (subtree)
          (if (pair? subtree)
              (square-tree subtree)
              (* subtree subtree)))
       tree))




(dn (square-tree
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7))))

(exit)
