(define (make-accumulator sum)
    (lambda (plus) (begin (set! sum (+ sum plus)) sum)))

(define A (make-accumulator 5))
(define B (make-accumulator 10))

(A 10)
(A 20)
(B 5)
(B 6)