
(define x (list (list 1 2) (list 3 4)))
(define xx (list x x))

(display x)
(newline)
(display xx)

(define (fringe lis)
  (define (iter items)
    (if (pair? items)
        (append (iter (car items))
              (iter (car (cdr items))))
        (list items)))
  (iter lis))

(newline)
(display (fringe x))
(newline)
(display (fringe xx))

(exit)
