(define (union-set set1 set2)
    (cond 
        ((null? set1) set2)
        ((null? set2) set1)
        (else
            (let ((x1 (car set1)) (x2 (car set2)))
                (cond 
                    ((< x1 x2) (cons x1 (union-set (cdr set1) set2)))
                    ((> x1 x2) (cons x2 (union-set set1 (cdr set2))))
                    (else (cons x1 (union-set (cdr set1) (cdr set2))))
                )
            )
        )
    )
)

(define (intersection-set set1 set2)
    (if (or (null? set1) (null? set2))
        '()
        (let ((x1 (car set1)) (x2 (car set2)))
            (cond 
                ((= x1 x2) (cons x1 (intersection-set (cdr set1) (cdr set2))))
                ((< x1 x2) (intersection-set (cdr set1) set2))
                ((< x2 x1) (intersection-set set1 (cdr set2)))))))

(union-set (list 2 4 5) (list 1 3 6))
(intersection-set (list 2 4 5) (list 1 4 6))