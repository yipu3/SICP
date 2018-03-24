(define (iterative-improve good-enough? improve)
    (lambda (guess) 
        (if (good-enough? guess)
            guess
            ((iterative-improve good-enough? improve) (improve guess)) 
        )
    )
)

(define (average x y)
    (/ (+ x y) 2))

(define (sqrt x) 
    (define (abs x)
        (if (> x 0)
            x
            (- x))
    )
    (define (square x) (* x x))
    (define (sqrt-enough guess)
        (<  (abs (- (square guess) x)) 0.001))

    (define (sqrt-improve guess) 
        (average guess (/ x guess)))
    ((iterative-improve sqrt-enough sqrt-improve) 1.0))
(sqrt 64.0)

(define (fixed-point f guess) 
    (define tolerance 0.000000001)
    (define (fp-enough? guess)
        (< (abs (- guess (f guess))) tolerance))
    (define (improve guess) (f guess))
    ((iterative-improve fp-enough? improve) guess)
)

(define (average-damp f) (lambda (x) (average x (f x))))
(define (s-3 x) (fixed-point (average-damp (lambda (y) (/ x (* y y)))) 1.0))
(s-3 27.0)