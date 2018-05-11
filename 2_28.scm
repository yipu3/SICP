(define (append a b)
    (if (null? a)
        b
        (cons (car a) (append (cdr a) b))
    )
)

(append (list 1 2) ())

(define (fringe items)
    (cond 
        ((null? items) items)
        ((not (pair? items)) (list items))
        (else (append (fringe (car items)) (fringe (cadr items))))
    )
)

(define x (list (list 1 2) (list 4 3)))
(fringe x)