(define (last-pair a)
    (if (null? (cdr a))
        (car a)
        (last-pair (cdr a))))

(last-pair (list 23 72 149 34))

(cons 1 (cons 2 (cons 3 '())))
(cons 1 '())
(list 1 4 9 16 25)

(define (append list1 list2)
    (if (null? list1)
        list2
        (cons (car list1) (append (cdr list1) list2))))

(append (list 1 2) (list 3 4))

(define (reverse a)
    (if (null? a)
        '()
        (append  (reverse (cdr a)) (list (car a))))
)

(reverse (list 1 4 9 16 25))

(pair? (1 2))
(pair? (2))
(null? ())

(define (deep-reverse tree)
    (newline)
    (display tree)
    (if (pair? tree)
        (if (null? (cdr tree))
            (deep-reverse (car tree))
            (list (deep-reverse (cdr tree)) (deep-reverse (car tree))))
        tree
    )
)

(define x (list (list 1 2) (list 3 4)))
x
(car x)
(deep-reverse x)