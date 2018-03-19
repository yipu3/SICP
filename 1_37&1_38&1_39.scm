(define (cont-frac n d k)
    (define (cont-frac-iter last k)
        (if (= k 0)
            last
            (cont-frac-iter (/ (n k) (+ (d k) last)) (- k 1))
        )
    )
    (cont-frac-iter 0 k)
)

(define (cont-frac-re n d k)
    (if (= k 0)
        0
        (/ (n k) (+ (d k) (cont-frac-re n d (- k 1))))
    )
)

(define (d i)
    (if (= 0 (remainder (- i 2) 3))
        (* (+ (/ (- i 2) 3) 1) 2)
        1)
)

(/ 1 (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 12))
(/ (cont-frac-re (lambda (i) 1.0) (lambda (i) 1.0) 12))

; ans = 12

(+ 2 (cont-frac (lambda (i) 1.0) d 12))

(define (lam-n x)
    (lambda (n) 
        (if (= n 1)
            x
            (* x x))))

(define (lam-d i)
    (- (* 2 i) 1))

(define (tan-cf x k)
    (cont-frac (lam-n x) lam-d k))

(tan-cf 1.5707963268 10000)