(define (average a b) (/ (+ a b) 2))

(define (average-damp f)
    (lambda (x) (average x (f x))))

(define tolerance 0.0001)

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

(define (sqrt x)
    (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))

(sqrt 4.0)

(define (deriv g)
    (lambda(x) (/ (- (g (+ x dx)) (g x)) dx)))

(define dx 0.00001)

(define (newton-transform g)
    (lambda (x) (- x (/ (g x) ((deriv g) x))))
)

(define (newtons-method g guess) 
    (fixed-point (newton-transform g) guess))

(define (square x) (* x x))
(square 1.5)

(define (sqrt x) (newtons-method (lambda (y) (- (square y) x)) 1.0))

(sqrt 4.0)
(sqrt 9.0)
(sqrt 2.25)

(define (cube x) (* x (square x)))

(define (cubic a b c) (lambda (x) (+ (* x x x) (* a x x)  (* b x) c)))

(newtons-method (cubic 1 1 1) 1.0)
(newtons-method (cubic 3 2 1) 1.0)
