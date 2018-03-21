(define (double pro)
    (lambda (x)(pro (pro x))))

(define (inc x) (+ x 1))
(inc 1)
((double inc) 1)
(((double (double double)) inc) 5)