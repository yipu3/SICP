(define (reverse tree)
    (newline)
    (display tree)
    (cond 
        ((not (pair? tree)) tree)
        ()
        (list (reverse (cdr tree)) (reverse (car tree)))
        tree)
)

(define x (list (list 1 2) (list 3 4)))
x
(car x)
(reverse x)