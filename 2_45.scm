(define (up-split painter n)
    (if (= n 0)
        painter
        (let ((smaller (up-split painter (- n 1))))
            (below painter (beside smaller smaller)))))

(define (split big_loc small_loc)
    (lambda (painter n)
        (if (= n 0)
            painter
            (let ((smaller ((split big_loc small_loc) painter (- n 1))))
                (big_loc painter (small_loc smaller smaller))))))