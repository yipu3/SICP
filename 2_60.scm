;O(N)
(define (element-of-set? x set)
    (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

;O(N)
(define (adjoin-set x set)
    (cons x set))

(define (erase x set)
    (cond 
        ((null? set) ())
        ((= x (car set)) (cdr set))
        (else (cons (car set) (erase x (cdr set))))))

;O(N^3)
(define (intersection-set set1 set2)
    (cond
        ((or (null? set1) (null? set2)) ())
        ((element-of-set? (car set1) set2)
            (cons (car set1) 
                (intersection-set (cdr set1) (erase (car set1) set2))))
        (else (intersection-set (cdr set1) set2))))

;O(N)
(define (union-set set1 set2)
    (append set1 set2)
)

(intersection-set (list 1 1 3 4) (list 1 1 3 4))
(union-set (list 1 1 3 4) (list 1 1 3 4))