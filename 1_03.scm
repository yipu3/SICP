(define (square x) (* x x))

(define (f x y z)
  (- (+ (square x) (square y) (square z)) (square (min x y z))))

(f 1 2 3)
