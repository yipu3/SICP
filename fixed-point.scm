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

(fixed-point cos 1.0)

(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2))

(define (improve-sqrt x y) (average y (/ x y)))
(define (improve-ocrt x y) (/ (+ (/ x (square y)) (* 2 y)) 3))

(define (normal f x)
    (fixed-point (set-improve f x) 1.0))

(define (set-improve f x)
    (lambda (y) (f x y))
)

(define (ocrt x)
    (normal improve-ocrt x)
    ;(fixed-point (lambda(y) (improve-ocrt x y)) 1.0)
)

(define (sqrt x)
    (normal improve-sqrt x)
)

(sqrt 4)
(sqrt 0.000004)
(ocrt 27)