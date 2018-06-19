(define (element-of-set? x set)
    (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(element-of-set? 1 (list 1 2))
(element-of-set? 3 (list 1 2))

(define (union-set set1 set2)
    (cond 
        ((null? set1) set2)
        ((null? set2) set1)
        ((not (element-of-set? (car set1) set2))
            (cons (car set1) (union-set (cdr set1) set2)))
        (else (union-set (cdr set1) set2))
    )
)

(union-set (list 1 2) (list 2 3))