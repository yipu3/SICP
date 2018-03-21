(define (average a b) (/ (+ a b) 2))

(define (average-damp f)
    (lambda (x) (average x (f x))))

(define tolerance 0.000000001)

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


(define (compose f g)
    (lambda (x) (f (g x))))

(define (repeated f n)
    (lambda (x)
        (if (= n 1)
            (f x)
            ((compose f (repeated f (- n 1))) x)
        )
    )
)

(define (n-mul y n)
    (if (= n 1)
        y
        (* y (n-mul y (- n 1)))))

(define (sqrt x) (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))
(sqrt 4.0)

(define (s-3 x) (fixed-point (average-damp (lambda (y) (/ x (* y y)))) 1.0))
(s-3 27.0)

(define (s-4 x) (fixed-point ((repeated average-damp 2) (lambda (y) (/ x (n-mul y 3)))) 1.0))
(s-4 81.0)

(define (s-5 x) (fixed-point ((repeated average-damp 2) (lambda (y) (/ x (n-mul y 4)))) 1.0))
(s-5 243.0)

(define (s-6 x) (fixed-point ((repeated average-damp 2) (lambda (y) (/ x (n-mul y 5)))) 1.0))
(s-6 729.0)

(define (s-7 x) (fixed-point ((repeated average-damp 2) (lambda (y) (/ x (n-mul y 6)))) 1.0))
(s-7 2187.0)

(define (s-8 x) (fixed-point ((repeated average-damp 3) (lambda (y) (/ x (n-mul y 7)))) 1.0))
(s-8 6561.0)

(define (qrt n x)
    (let ((damp (+ (ceiling (/ n 4)) 1))
          (y-low (- n 1)))
        (fixed-point ((repeated average-damp damp) (lambda (y) (/ x (n-mul y y-low)))) 1.0))
)

(qrt 4 81.0)
(qrt 5 243.0)
(qrt 8 6561.0)