(define (dn x)
  (display x)
  (newline))

(define true #t)

(define false #f)

; **********************

(define global-array '())

(define (make-entry k v) (list k v))
(define (key entry) (car entry))
(define (value entry) (cadr entry))

(define (put op type item)
  (define (put-helper k array)
    (cond ((null? array) (list(make-entry k item)))
          ((equal? (key (car array)) k) array)
          (else (cons (car array) (put-helper k (cdr array))))))
  (set! global-array (put-helper (list op type) global-array)))

(define (get op type)
  (define (get-helper k array)
    (cond ((null? array) #f)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))
  (get-helper (list op type) global-array))

(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

; ***********************

; a:
(define (install-cp1-package)
  (define (get-record file name)
    'info)
  (define (get-salary file name)
    'salary)
  (put 'get-record 'cp1 get-record)
  (put 'get-salary 'cp1 get-salary)
  'done)

(define (get-record file name)
  ((get 'get-record (type-tag file)) (contents file) name))

(install-cp1-package)
(dn (get-record (attach-tag 'cp1 'file) 'sb))

; b:
(define (get-salary file name)
  ((get 'get-salary (type-tag file)) (contents file) name))
(dn (get-salary (attach-tag 'cp1 'file) 'sb))

; c:
(define (find-employee-record files name)
  (if (null? files)
      #f
      (let ((record (get-record (car files) name)))
           (if record
               record
               (find-employee-record (cdr files) name)))))

(dn (find-employee-record (list (attach-tag 'cp1 'file)) 'sb))

; d:
; 让新公司提供一个查询包
(exit)
