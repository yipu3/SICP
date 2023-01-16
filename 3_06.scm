(define (rand-update x)
    (+ x 1))

(define (make-rand random-num)
    (define (generate)
        (begin (set! random-num (rand-update random-num)) random-num))
    (define (reset reset-num)
        (begin (set! random-num reset-num) random-num))
    (define (dispatch op)
        (display op)
        (cond 
            ((eq? op 'generate) generate)
            ((eq? op 'reset) reset)
            (else (error "Unknown request == MAKE_RAND" 
                op))))
    dispatch)

(define a (make-rand 0))

((a 'generate))
((a 'generate))
((a 'generate))
((a 'reset) -1)
((a 'generate))
((a 'generate))
((a 'generate))