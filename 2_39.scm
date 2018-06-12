(define (fold-right op init l)
    (if (null? l)
        init
        (op (car l) (fold-right op init (cdr l)))))

(define (fold-left op initial sequence)
    (define (iter result rest)
        (if (null? rest)
            result
            (iter (op result (car rest)) (cdr rest))))
    (iter initial sequence)
)

(define (reverse seq) (fold-right (lambda (x y) (append y (list x))) () seq))
(reverse (list 1 2 3))
(define (reverse seq) (fold-left (lambda (x y) (cons y x)) () seq))
(reverse (list 1 2 3))