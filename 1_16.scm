(define (even? n) (= (remainder n 2) 0))
(define (square n) (* n n))

(define (f-iter a b n)
  (cond ((= n 0) a)
        ((even? n) (f-iter a (square b) (/ n 2)))
        (else (f-iter (* a b) b (- n 1)))))

(define (exp b n) (f-iter 1 b n))

(exp 3 2)
