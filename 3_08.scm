(define make-f
    (let ((result 1))
        (lambda (x)
        (begin (set! result (- result x)) result))))

(define f make-f)

(+ (f 1) (f 0))
;(+ (f 0) (f 1))