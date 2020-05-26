(define (dn x)
  (display x)
  (newline))

; **********************

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define with-cycle (make-cycle (cons 1 (cons 1 (cons 2 '())))))
(define without-cycle (list 1 2 3 4 5 6))

; **********************

(define (check-cycle x)
  (define (include? path node)
    (cond ((null? path) #f)
          (else (if (eq? (car path) node)
                    #t
                    (include? (cdr path) node)))))
  (define (iter path node)
    (if (not (pair? node))
        #f
        (if (include? path node)
            #t
            (let ((new-path (cons node path)))
                 (or (iter new-path (car node))
                     (iter new-path (cdr node)))))))
  (iter '() x))

(dn (check-cycle with-cycle))
(dn (check-cycle without-cycle))


(exit)
