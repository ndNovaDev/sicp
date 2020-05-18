(define (dn x)
  (display x)
  (newline))

(define nil '())

(define true #t)

(define false #f)

; **********************

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
                (choose-branch (car bits) current-branch)))
              (if (leaf? next-branch)
                  (cons (symbol-leaf next-branch)
                        (decode-1 (cdr bits) tree))
                  (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "sb" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))
                    
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
           (adjoin-set (make-leaf (car pair)
                                  (cadr pair))
                       (make-leaf-set (cdr pairs))))))
; **********************

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree (make-leaf 'D 1)
                                    (make-leaf 'C 1)))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (next-node symbol tree)
  (let ((lb (left-branch tree))
        (rb (right-branch tree)))
       (cond ((element-of-set? symbol (symbols lb)) (cons 0 lb))
             ((element-of-set? symbol (symbols rb)) (cons 1 rb))
             (else (error "sb" symbol)))))

(define (encode-symbol symbol tree)
  (define (iter ret tree)
    (if (null? tree)
        ret
        (let ((node (next-node symbol tree)))
             (if (leaf? (cdr node))
                 (cons (car node) '())
                 (cons (car node) (encode-symbol symbol (cdr node)))))))
  (iter '() tree))

; (dn (encode-symbol 'D sample-tree))
; (dn (encode-symbol 'A sample-tree))
; (dn (encode-symbol 'hello sample-tree) )

(dn (encode '(A D A B B C A) sample-tree))
; (dn (encode (list 'A 'D 'A 'B 'B 'C 'A) sample-tree))
; (0 1 1 0 0 1 0 1 0 1 1 1 0)
(exit)
