(define (f n)
    (
        if (< n 3)
        n
        (
            + (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3)))
        )
    )
)

(define (f-iter step deep plusa plusb plusc)
    (if (< 3 deep)
        deep
        (if (= step deep)
            (+ plusa plusb plusc)
            (if (< step 3)
                (f-iter (+ step 1) 
                        deep 
                        step
                        (* 2 plusa)
                        (* (/ plusb 2) 3)
                )
                (f-iter (+ step 1) 
                        deep 
                        (+ plusa plusb plusc)
                        (* 2 plusa)
                        (* (/ plusb 2) 3)
                )
            )
        )
    )
)
(define (f n)
    (f-iter 0 n 0 0 0)
)