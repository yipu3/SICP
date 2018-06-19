(define (equal? a b)
    (if (pair? a)
        (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b)))
        (eq? a b)))

(equal? '(this is a list) '(this is a list))
(equal? '(this is a list) '(this (is a) list))