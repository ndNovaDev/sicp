(define (dn x)
  (display x)
  (newline))

(define nil '())

(define true #t)

(define false #f)

; **********************

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                   (if (eq? type1 type2)
                       (error "sb", type1)
                       (let ((t1->t2 (get-coercion type1 type2))
                            (t2->t1 (get-coercion type2 type1)))
                        (cond (t1->t2
                                (apply-generic op (t1->t2 a1) a2))
                              (t2->t1
                                (apply-generic op a1 (t2->t1 a2)))
                              (else
                                (error "sb" (list op type-tags)))))))
              (error "sb" (list op type-tags)))))))

; ***********************

(define (apply-generic op . args)
  (define (parse target)
    (map (lambda (x)
          (let ((coercor (get-coercion (type-tag x) (type-tag target))))
               (if coercor
                  (coercor x)
                  x)))
         args))
  (define (iter next)
    (if (null? next)
        (error "sb" next)
        (let ((parsed-args (parse (car next))))
             (let ((proc (get op (type-tags parsed-args))))
                  (if proc
                      (apply proc (map contents parsed-args))
                      (iter (cdr next)))))))
  (let ((type-tags (map type-tag args)))
       (let ((proc (get op type-tags)))
            (if proc
                (apply proc (map contents))
                (iter args)))))

; http://community.schemewiki.org/?sicp-ex-2.82
(exit)
