(define tolerance 0.0001)

(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
        (< (/ (abs (- v1 v2)) v2) tolerance))
    (define (try guess)
        (let ((next (f guess)))
            (if
                (close-enough? guess next) 
                    next
                    (try next))))
(try first-guess))

(define (improve-hj x) (+ 1 (/ 1 x)))
(define (get-hj) (fixed-point improve-hj 1.0))
(get-hj)