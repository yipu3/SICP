(define (double a) (+ a a))

(define (halve a) (/ a 2))

(define (even? n) (= (remainder n 2) 0))

(define (f-iter s a b)
  (cond
    ((= b 0) s)
    ((even? b) (f-iter s (double a) (halve b)))
    (else (f-iter (+ s a) a (- b 1)))))

(define (* a b) (f-iter 0 a b))

(* 19 100)
