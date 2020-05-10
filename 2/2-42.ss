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

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (positions) (safe? k positions))
          (flatmap
            (lambda (rest-of-queens)
              (map (lambda (new-row)
                    (adjoin-position new-row k rest-of-queens))
                   (enumerate-interval 1 board-size)))
            (queen-cols (- k 1))))))
  (queen-cols board-size))
; ***********************


(define empty-board nil)

(define (safe? k positions)
  (define (proc v pos index)
    (if (null? pos)
        #t
        (let ((tar (car pos))
              (top (- v index))
              (bottom (+ v index)))
             (and (not (= v tar))
                  (not (= top tar))
                  (not (= bottom tar))
                  (proc v (cdr pos) (+ index 1))))))
  (proc (car positions) (cdr positions) 1))
(define (adjoin-position row col qs)
  (cons row qs))

(dn empty-board)
(dn (queens 8))
(exit)
