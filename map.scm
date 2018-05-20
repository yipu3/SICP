(define (append a b)
    (if (null? a)
        b
        (cons (car a) (append (cdr a) b))))

(append (list ) (list 1 2))

(define (map p l)
    (define (map-iter p left-l right-l)
        (newline)
        (display left-l)
        (if (null? right-l)
            left-l
            (map-iter p (append left-l (list (p (car right-l)))) (cdr right-l))))
    (map-iter p () l)
)

(for-each display (list 1 2 3 4))