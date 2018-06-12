(define (accumulate op init l)
    (if (null? l)
        init
        (op (car l) (accumulate op init (cdr l)))))
    
(define (accumulate-n op init seqs)
    (if (null? (car seqs))
        '()
        (cons 
            (accumulate op init (map car seqs)) 
            (accumulate-n op init (map cdr seqs))
        )
    )
)

(define (dot-product v w)
    (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
    (map (lambda (x) (dot-product x v)) m))

(define (transpose m)
    (accumulate-n cons '() m))

(define (matrix-*-matrix m n)
    (let ((cols (transpose n)))
        (map dot-product m cols)
    )
)

(define m (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
(transpose m)
(matrix-*-matrix m (transpose m))