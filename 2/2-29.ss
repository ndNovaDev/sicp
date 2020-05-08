(define (dn x)
  (display x)
  (newline))

(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))
; ************************************
; a
(define (left-branch m)
  (car m))
(define (right-branch m)
  (car (cdr m)))
(define (branch-length b)
  (car b))
(define (branch-structure b)
  (car (cdr b)))
; test
(define mobile (make-mobile (make-branch 10 25)
                            (make-branch 5 20)))
(dn (left-branch mobile))
(dn (right-branch mobile))
(dn (branch-length (right-branch mobile)))
(dn (branch-structure (right-branch mobile)))

; b
(define (left-branch-structure m)
  (branch-structure (left-branch m)))
(define (right-branch-structure m)
  (branch-structure (right-branch m)))
(define (structure-weight s)
  (if (pair? s)
      (+ (structure-weight (left-branch-structure s))
         (structure-weight (right-branch-structure s)))
      s))
(define (total-weight m)
  (structure-weight m))
; test
(dn (total-weight mobile))
(define another-mobile (make-mobile (make-branch 10 mobile)
                                    (make-branch 10 20)))
(dn (total-weight another-mobile))

; c
(define (balance? m)
  (if (pair? m)
      (let ((lb (left-branch m))
            (rb (right-branch m)))
           (let ((ll (branch-length lb))
                 (rl (branch-length rb))
                ;  (branch-structure lb)重复了，但是懒得改
                 (lv (total-weight (branch-structure lb)))
                 (rv (total-weight (branch-structure rb))))
                (and (= (* ll lv)
                        (* rl rv))
                     (balance? (branch-structure lb))
                     (balance? (branch-structure rb)))))
      #t))
; test
(define balance-mobile (make-mobile (make-branch 10 10)
                                    (make-branch 10 10)))
(dn (balance? balance-mobile))
(define unbalance-mobile (make-mobile (make-branch 0 0)
                                      (make-branch 10 10)))
(dn (balance? unbalance-mobile))
(define mobile-with-sub-mobile (make-mobile (make-branch 10 balance-mobile)
                                            (make-branch 10 balance-mobile)))
(dn (balance? mobile-with-sub-mobile))

; d
; 只需改写a中的选择函数
(exit)
