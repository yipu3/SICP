(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

; if x is a very small number, such as 0.0004
; add good enough guess's square can be 0.00139
; which is far beyond 0.0004
; because the delta 0.001 is greater than 0.0004

; if x is a very large number. For large radicands, 
; the procedure sqrt-iter enters an infinite recursion
; because the tolerance is not scaled up to the large radicands
; and floating-point numbers are represented with limited precision
; so the absolute error at that scale is always greater than the tolerance.
; (define (good-enough? guess x)
;   (< (abs (- (square guess) x)) 0.001))

(define (good-enough? guess x)
  (< (/ (abs (- (square guess) x)) x) 0.001))

(define (sqrt x) (sqrt-iter 1.0 x))

(sqrt 0.0004)
(sqrt 1e-308)
