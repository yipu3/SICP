(define make-point cons)
(define  (x-point p) (car p))
(define  (y-point p) (cdr p))

(define (make-segment pa pb) (cons pa pb))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))

(define (print-point p)
    (newline)
    (display "(")
    (display (x-point p))
    (display ",")
    (display (y-point p))
    (display ")")
)

(define (print-seg seg) 
    (newline)
    (print-point (start-segment seg))
    (print-point (end-segment seg))
)

(define pa (make-point 1 2))
(define pb (make-point 4 5))

(print-point pa)
(print-point pb)

(define sega (make-segment pa pb))
(print-seg sega)

(define (rec pa pb) (cons pa pb))
(define (get-width reca) (abs (- (x-point pa) (x-point pb))))
(define (get-height reca) (abs (- (y-point pa) (y-point pb))))

(define (zhouchang rec) (+ (* 2 (get-width rec)) 
                           (* 2 (get-height rec))))
(define (mianji rec) (* (get-height rec) (get-width rec)))

(define reca (rec pa pb))
(zhouchang reca)
(mianji reca)

(define (rec p width height) (cons p (cons width height)))
(define (get-width reca) (car (cdr reca)))
(define (get-height reca) (cdr (cdr reca)))

(define reca (rec pa 3 3))
(zhouchang reca)
(mianji reca)