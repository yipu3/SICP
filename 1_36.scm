(define tolerance 0.000001)

(define (counter count)
    (define (plus1) (set! count (+ 1 count)) count)
    plus1
)

(define (fixed-point f first-guess count)
    (define (close-enough? v1 v2)
        (< (/ (abs (- v1 v2)) v1) tolerance))
    (define (try guess)
        (count)
        (let ((next (f guess)))
            (if
                (close-enough? guess next) 
                    next
                    (try next))))
(try first-guess))

(define (guess-x x) (/ (log 1000) (log x)))
(define (average x y) (/ (+ x y) 2))
(define (improve-pjzn x) (average x (guess-x x)) )
(define (improve2 x) (guess-x x) )

(define ca (counter 0))
(define cb (counter 0))
(fixed-point improve-pjzn 2.0 ca)
(fixed-point improve2 2.0 cb)
;(fixed-point2 improve 2.0)
(ca)
(cb)