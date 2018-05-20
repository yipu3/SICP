(define (accumulate op init l)
    (if (null? l)
        init
        (op (car l) (accumulate op init (cdr l)))))

(define (length sequence)
    (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(define (count-leaves t)
    (accumulate + 0 (map (lambda (x) (if (pair? x) (count-leaves x) 1)) t)))

(define x (cons (list 1 2) (list 3 4)))
(count-leaves x)