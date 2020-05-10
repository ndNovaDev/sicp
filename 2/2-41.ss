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

(define (unique3 n)
  (flatmap (lambda (x)
                   (map (lambda (p)
                                (append p
                                        (list x)))
                        (unique-pairs (- x 1))))
           (enumerate-interval 1 n)))

(define (proc n s)
  (filter (lambda (p)
                  (let ((x (car p))
                        (y (cadr p))
                        (z (caddr p)))
                       (= s
                          (+ x y z))))
          (unique3 n)))

(dn (proc 10 8))
(exit)
