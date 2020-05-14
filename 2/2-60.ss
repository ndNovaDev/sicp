(define (dn x)
  (display x)
  (newline))
(define nil '())
(define true #t)
(define false #f)

; ********************

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) nil)
        ((and (element-of-set? (car set1) set2) 
              (not (element-of-set? (car set1) (cdr set1))))
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(dn (intersection-set (list 5 5 5 5 5 1 2 3 9 9 9) (list 4 5 6 2 2 3 3 4 5 6 6 )))

; (define (union-set set1 set2)
;   (cond ((null? set1) set2)
;         ((null? set2) set1)
;         ((element-of-set? (car set1) set2)
;          (union-set (cdr set1) set2))
;         (else (cons (car set1)
;                     (union-set (cdr set1) set2)))))
(define (union-set set1 set2)
  (define (iter ret set)
    (if (null? set)
        ret
        (let ((value (car set)))
         (if (element-of-set? value ret)
             (iter ret (cdr set))
             (iter (cons value ret) (cdr set))))))
  (iter nil (append set1 set2)))

(dn (union-set (list 1 3 5 5 5 5 5 5 5) (list 1 2 3 )))

(exit)
