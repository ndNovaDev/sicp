(define (dn x)
  (display x)
  (newline))

(define nil '())

(define true #t)

(define false #f)

; **********************

 (define (raise x) (apply-generic 'raise x)) 
  
 ;; add into scheme-number package 
 (put 'raise 'integer  
          (lambda (x) (make-rational x 1))) 
  
 ;; add into rational package 
 (put 'raise 'rational 
          (lambda (x) (make-real (/ (numer x) (denom x))))) 
  
 ;; add into real package 
 (put 'raise 'real 
          (lambda (x) (make-from-real-imag x 0))) 

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

; **************************

(define (apply-generic op . args)
  (define (try-raise type1 type2 a1 a2)
    (let ((r1 (get 'raise type1))
          (r2 (get 'raise type2)))
         ()))
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
                         (error "sb" type1)
                         (try-raise type1 type2 a1 a2)))
                (error "sb" args))))))

(exit)
