(define (enumerate-interval low high)
    (if (>= low high)
        (list low)
        (cons low (enumerate-interval (+ low 1) high))))

(enumerate-interval 1 1)

(define (flatmap proc seq) (accumulate append '() (map proc seq)))

(define (accumulate proc init seq)
    (if (null? seq)
        init
        (proc (car seq) (accumulate proc init (cdr seq)))))

(define (conflict? ay ax by bx)
    (or (= ax bx) (= ay by) (= (abs (- ax bx)) (abs (- ay by)))))

(define (length l)
    (if (null? l)
        0
        (+ 1 (length (cdr l)))))

(define (safe? k positions)
    (define (safe?-inner now_pos rest-positions now_y old_y)
        (if (null? rest-positions)
            #t
            (if (conflict? old_y (car rest-positions) now_y now_pos)
                #f
                (safe?-inner now_pos (cdr rest-positions) now_y (- old_y 1))
            )
        )
    )
    (safe?-inner (car positions) (cdr positions) k (- k 1))
)

(safe? 2 (list 0 2))
(safe? 1 (list 0))
(safe? 2 (list 0))

(define (adjoin-position new-row k rest-of-queens)
    (append (list new-row) rest-of-queens))

(adjoin-position 2 1 (list 0))

(define empty-board '())
(list empty-board)

(define (queens board-size)
    (define (queen-cols k)
        (if (= k 0)
            (list empty-board)
            (filter
                (lambda (positions) (safe? k positions))
                (flatmap
                    (lambda (rest-of-queens) 
                        (map (lambda (new-row) 
                            (adjoin-position new-row k rest-of-queens))
                            (enumerate-interval 1 board-size)))
                            (queen-cols (- k 1))))))
    (queen-cols board-size))

(queens 0)
(queens 1)
(queens 2)
(queens 3)
(queens 4)
(queens 8)