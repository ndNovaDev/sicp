(define (dn x)
  (display x)
  (newline))

(define nil '())

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval x y)
  (if (> x y)
      nil
      (cons x (enumerate-interval (+ x 1) y))))


(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

; ***********************

(define (unique-pairs n)
  (flatmap (lambda (x)
                   (map (lambda (y)
                                (list y x))
                        (enumerate-interval 1 (- x 1))))
           (enumerate-interval 1 n)))

(dn (unique-pairs 5))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))
(exit)
