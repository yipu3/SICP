(define make-vec cons)

(define (xcor-vect vec)
    (car vec))

(define (ycor-vect vec)
    (cdr vec))

(define (add-vect a b)
    (make-vec (+ (xcor-vect a) (xcor-vect b)) (+ (ycor-vect a) (ycor-vect b))))

(define (sub-vect a b)
    (make-vec (- (xcor-vect a) (xcor-vect b)) (- (ycor-vect a) (ycor-vect b))))

(define (scale-vect s vec)
    (make-vec (* (xcor-vect vec) s) (* (ycor-vect vec) s)))