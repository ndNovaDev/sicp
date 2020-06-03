(define (dn x)
  (display x)
  (newline))

(define true #t)

(define false #f)

; **********************

(define theEmptyStream '())

(define (streamNull s)
  (null? '()))

(define (streamRef s n)
  (if (= n 0)
      (streamCar s)
      (streamRef (streamCdr s) (- n 1))))

(define (streamMap proc s)
  (if (streamNull s)
      theEmptyStream
      (consStream (proc (streamCar s))
                  (streamMap proc (streamCdr s)))))

(define (streamForEach proc s)
  (if (streamNull s)
      'done
      (begin (proc (streamCar s))
             (streamForEach proc (streamCdr s)))))

(define (displayStream s)
  (streamForEach dn s))

(define (consStream a b)
  (cons a (delay b)))

(define (streamCar s) (car s))

(define (streamCdr s) (force (cdr s)))

(define (streamEnumerateInterval low high)
  (if (> low high)
      theEmptyStream
      (consStream
       low
       (streamEnumerateInterval (+ low 1) high))))

(define (streamFilter pred s)
  (cond ((streamNull s) theEmptyStream)
        ((pred (streamCar s))
         (consStream (streamCar s)
                     (streamFilter pred
                                   (streamCdr s))))
        (else (streamFilter pred (streamCdr s)))))

(define (force delayedObject)
  (delayedObject))

(define (memoProc proc)
  (let ((alreadyRun false) (result false))
    (lambda ()
      (if (not alreadyRun)
          (begin (set! result (proc))
                 (set! alreadyRun true)
                 result)
          result))))

(define (delay exp)
  (memoProc (lambda () exp)))

; *********************

(define (streamMap proc . argstrams)
  (if (streamNull (car argstrams))
      theEmptyStream
      (consStream
        (apply proc (map streamCar argstrams))
        (apply streamMap
               (cons proc (map streamCdr argstrams))))))

(exit)
