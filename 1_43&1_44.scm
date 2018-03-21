(define (square x) (* x x))

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

((repeated square 2) 5)

(define (smooth g) (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (n-smooth g n) (lambda (g) ((repeated smooth n) g))