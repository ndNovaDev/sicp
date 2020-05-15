(define (dn x)
  (display x)
  (newline))
(define nil '())
(define true #t)
(define false #f)

; **********************

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      nil
      (let ((x1 (car set1)) (x2 (car set2)))
           (cond ((= x1 x2)
                  (cons x1
                        (intersection-set (cdr set1)
                                          (cdr set2))))
                 ((< x1 x2)
                  (intersection-set (cdr set1) set2))
                 ((< x2 x1)
                  (intersection-set set1 (cdr set2)))))))

; **********************

(define (adjoin-set x set)
  (define (iter x set)
    (if (null? set)
        (list x)
        (let ((next (car set)))
         (if (< x next)
             (cons x set)
             (cons next (iter x (cdr set)))))))
  (if (element-of-set? x set)
      set
      (iter x set)))

(define set (list 2 3 5))
(dn (adjoin-set 1 set))
(dn (adjoin-set 4 set))
(dn (adjoin-set 10 set))


(exit)
