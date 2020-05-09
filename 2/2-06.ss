
(define (p n)
  (display "*"))

(define zero (lambda (f) (lambda (x) x))) 

(define (add-1 n) 
   (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define three (lambda (f) (lambda (x) (f (f (f x))))))

(define (add a b) 
   (lambda (f) 
     (lambda (x) 
       ((a f) ((b f) x))))) 

(define five (add two three))

((five p) "*")

(exit)
