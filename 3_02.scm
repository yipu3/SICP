(define (make-monitored func)
    (let ((cnt 0))
        (lambda (x) (if (eq? x 'how-many-calls?)
            cnt
            (begin (set! cnt (+ cnt 1)) func)))
    )
)

(define s
    (make-monitored sqrt))

(s 100)
(s 'how-many-calls?)