(define (dn x)
  (display x)
  (newline))

; **********************

; (define (count-pairs x)
;   (define l '())
;   (define (store x)
;     (set! l (cons x l)))
;   (define (dul? x l)
;     (if (null? l)
;         #f
;         (if (eq? (car l) x)
;             #t
;             (dul? x (cdr l)))))
;   (define (iter x)
;     (cond ((not (pair? x)) 0)
;           ((dul? x l) 0)
;           (else (+ (count-pairs (car x))
;                    (count-pairs (cdr x))
;                    1)))))

(define (count-pairs x)
  (define l '())
  (define (store x)
    (set! l (cons x l)))
  (define (dul? x l)
    (if (null? l)
        #f
        (if (eq? (car l) x)
            #t
            (dul? x (cdr l)))))
  (define (iter x)
    (cond ((not (pair? x)) 0)
          ((dul? x l) 0)
          (else (begin (store x)
                       (+ (iter (car x))
                       (iter (cdr x))
                       1)))))
  (iter x))

; (define test1 (list 1 2 3 4 5))
; (dn (count-pairs test1))

(define base (cons 1 2))
(define test (cons base (cons 2 base)))
(dn test)
(dn (count-pairs test))

(exit)
