(define (cons x y)
    (lambda (m) (m x y)))

(define (car z)
    (z (lambda (p q) p)))

(define (cdr z)
    (z (lambda (p q) q)))

(define (ci di mi)
    (if (= mi 1)
        di
        (* di (ci di (- mi 1)))))

(define (cons x y)
    (* (ci 2 x) (ci 3 y)))

(define (chu num bc)
    (if (= (remainder num bc) 0)
        (+ 1 (chu (/ num bc) bc))
        0))

(define (car r) (chu r 2))
(define (cdr r) (chu r 3))

(define temp (cons 22 33))
(define (print-cons r)
    (newline)
    (display (car r))
    (display ":")
    (display (cdr r)))

(print-cons temp)