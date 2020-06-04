(define (dn x)
  (display x)
  (newline))

(define true #t)

(define false #f)

; **********************

(define (displayLine x)
  (newline)
  (display x))

(define theEmptyStream '())

(define (streamNull s)
  (null? s))

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
  (streamForEach displayLine s))

(define (delay exp)
  (memoProc (lambda () exp)))

(define (consStream a b)
  (cons a (delay b)))

(define-syntax consStream
  (syntax-rules ()
    ((consStream a b)
     (cons a (memoProc (lambda () b))))))

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


(define (integersStartingFrom n)
  (consStream n (integersStartingFrom (+ n 1))))

(define (divisible x y) (= (remainder x y) 0))

(define (addStreams s1 s2)
  (streamMap + s1 s2))

(define (scaleStream stream factor)
  (streamMap (lambda (x) (* x factor)) stream))


; *********************

(define (sqrtImprove guess x)
  (/ (+ guess (/ x guess)) 2))
(define (sqrtStream x)
  (define guesses
    (consStream 1.0
                (streamMap (lambda (guess)
                              (sqrtImprove guess x))
                           guesses)))
  guesses)

; *********************

(define (pairs s t)
  (consStream
    (list (streamCar s) (streamCar t))
    (interleave
      (streamMap (lambda (x) (list (streamCar s) x))
                 (streamCdr t))
      (pairs (streamCdr s) t))))

(exit)
