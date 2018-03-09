(define (new-if predicate then-clause else-clause) 
 (cond (predicate then-clause) 
 (else else-clause)))

(new-if (= 2 3) 0 5)
