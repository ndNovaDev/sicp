(define (dn x)
  (display x)
  (newline))
(define false #f)
(define true #t)

; **********************

; parallelExecute
; makeSerializer

(define (makeSerializer)
  (let ((mutex (makeMutex)))
    (lambda (p)
      (define (serializedP .args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serializedP)))

(define (makeMutex)
  (let ((cell (list false)))
    (define (theMutex m)
      (cond ((eq? m 'acquire)
             (if (testAndSet cell)
                 (theMutex 'acquire)))
            ((eq? m 'release) (clear cell))))
    theMutex))

(define (clear cell)
  (set-car! cell false))

(define (testAndSet cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))

; ************************

pass

(exit)
