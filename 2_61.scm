(define (adjoin-set x set)
    (cond 
        ((null? set) (list x))
        ((< (car set) x) (cons (car set) (adjoin-set x (cdr set))))
        (else (cons x set))
    )
)

(adjoin-set 4 (list 1 3 5))