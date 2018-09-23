(define (randon-in-range low high)
    (let ((range (- high low)))
        (+ low (random range))))

(define (monte-carlo trials experiment)
    (define (iter trials-remaining trials-passed)
        (cond 
            ((= trials-remaining 0) (/ trials-passed trials))
            ((experiment) 
                (iter (- trials-remaining 1) (+ trials-passed 1)))
            (else 
                (iter (- trials-remaining 1) trials-passed))))
    (iter trials 0))

(define (estimate-integral P x1 x2 y1 y2 trials-count)
    (define (test)
        (let ((x (randon-in-range x1 x2)) (y (randon-in-range y1 y2)))
            ;(display (list x y))
            (P x y)))
    (monte-carlo trials-count test))

(define (in-circle x y)
    (< (+ (square x) (square y)) 1))

(exact->inexact (* 4 (estimate-integral in-circle -1.0 1.0 -1.0 1.0 10000000)))