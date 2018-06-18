(define (make-frame origin edge1 edge2)
    (list origin edge1 edge2))

(define (origin-frame frame)
    (car frame))

(define (edge1-frame frame)
    (cadr frame))

(define (edge2-frame frame)
    (caddr frame))

(define x (make-frame 0 1 2))
(origin-frame x)
(edge1-frame x)
(edge2-frame x)

(define (make-frame origin edge1 edge2)
    (cons origin (cons edge1 edge2)))

(define (origin-frame frame)
    (car frame))

(define (edge1-frame frame)
    (cadr frame))

(define (edge2-frame frame)
    (cddr frame))

(define x (make-frame 0 1 2))
(origin-frame x)
(edge1-frame x)
(edge2-frame x)