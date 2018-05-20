(define (accumulate op init l)
    (if (null? l)
        init
        (op (car l) (accumulate op init (cdr l)))))

(define (map p sequence)
    (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(map square (list 1 2 3 4))

(define (append seq1 seq2)
    (accumulate cons seq2 seq1))

(append (list 1 2 3 4) (list 5 6 7))

(define (length sequence)
    (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(length (list 1 2 3 4))