(define (dn x)
  (display x)
  (newline))

; **********************

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "sb" "sb"))))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
         (after-delay inverter-delay
                      (lambda ()
                              (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)
(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-valie
            (logical-and (get-signal a1) (get-signal a2))))
         (after-delay and-gate-delay
                      (lambda ()
                        (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
            (logical-or (get-signal a1) (get-signal a1))))
         (after-delay or-gate-delay
                      (lambda ()
                        (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
       (or-gate a b d)
       (and-gate a b c)
       (inverter c e)
       (and-gate d e s)
       'ok))
(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
       (half-adder b c-in s c1)
       (half-adder a s sum c2)
       (or-gate c1 c2 c-out)
       'ok))

; *******************************

(define (or-gate a1 a2 output)
  (let ((av1 (make-wire))
        (av2 (make-wire)
        (ba (make-wire))))
       (inverter a1 av1)
       (inverter a2 av2)
       (and-gate av1 av2 ba)
       (inverter ba output)))

(exit)
