(define (f x y)
  (cond
    ((= y 0) 1)
    ((= x y) 1)
    (else (+ (f (- x 1) (- y 1)) (f (- x 1) y)))))

(f 0 0)
(f 1 0)
(f 1 1)
(f 2 1)
(f 4 3)
