(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2)) tolerance))
    (define (try guess)
        (let ((next (f guess)))
            (if
                (close-enough? guess next) 
                    next
                    (try next))))
(try first-guess))

(define tolerance 0.000001)
(fixed-point cos 1.0)

(define (average x y) (/ (+ x y) 2))
(define (foo x) (average x (/ 2 x)))
(fixed-point foo 1.0)
