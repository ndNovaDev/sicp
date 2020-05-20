(define (dn x)
  (display x)
  (newline))

(define nil '())

(define true #t)

(define false #f)

; **********************

; type-tag
; contents
; attach-tag


; (define (attach-tag type-tag contents)
;   (cons type-tag contents))
(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

; (define (type-tag datum)
;   (if (pair? datum)
;       (car datum)
;       (error "sb" datum)))
(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "sb" datum))))

; (define (contents datum)
;   (if (pair? datum)
;       (cdr datum)
;       (error "sb" datum)))
(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "sb" datum))))


(exit)
