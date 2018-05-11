(define (square a)
    (* a a))

(define (square-list l)
    (if (null? l)
        '()
        (cons (square (car l)) (square-list (cdr l)))
    )
)

(square-list (list 1 2 3 4))

(define (square-list l)
    (map square l)
)

(square-list (list 1 2 3 4))

(define (square-list items)
    (define (iter things answer)
        (if (null? things)
            answer
            (iter (cdr things)
                  (cons answer 
                      (square (car things)))))
    )
    (iter items '())
)

(square-list (list 1 2 3 4))

(define (for-each func items)
    (func (car items))
    (if (null? (cdr items))
        true
        (for-each func (cdr items))
    )
)

(for-each (lambda (x) (newline) (display x)) (list 57 321 88))