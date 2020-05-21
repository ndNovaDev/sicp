(define (dn x)
  (display x)
  (newline))

(define nil '())

(define true #t)

(define false #f)

; **********************

(define (make-monitored f)
  (define count 0)
  (define (call-f args)
    (begin (set! count (+ count 1))
           (apply f args)))
  (define (how-many-calls?)
    count)
  (define (reset-count)
    (!set count 0))
  (lambda (first . args)
    (cond ((eq? 'how-many-calls? first)
                (how-many-calls?))
               ((eq? 'reset-count first)
                (reset-count))
               (else (call-f (cons first args))))))

(define s (make-monitored sqrt))
(dn (s 100))
(dn (s 'how-many-calls?))

(exit)
