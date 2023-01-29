(define (cube x) (* x x x))
(define (p x) (begin (display 1) (- (* 3 x) (* 4 (cube x)))))
(define (sine angle)
   (if (not (> (abs angle) 0.1))
       angle
       (p (sine (/ angle 3.0)))))

(sine 12.15)

; 5 times
; space: O(1)
; number of steps: O(log3 n)