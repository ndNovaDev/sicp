
(define (reverse l)
  (define (iter ret ori)
    (if (null? ori)
        ret
        (let ((value (car ori))
              (next (cdr ori)))
             (iter (cons value ret) next))))
  (iter (list (car l)) (cdr l)))

(display (reverse (list 1 4 9 16 25)))

(exit)
