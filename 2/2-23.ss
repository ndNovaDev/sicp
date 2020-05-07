

(define (for-each p items)
  (cond ((not (null? items)) (p (car items)) (for-each p (cdr items)))))

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))

(exit)


(cond (predicate1 consequent1)
  (predicate2 consequent2))