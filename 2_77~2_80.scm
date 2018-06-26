;;基础操作，操作-类型表
(define global-array '())

(define (make-entry k v) (list k v))
(define (key entry) (car entry))
(define (value entry) (cadr entry))

(define (put op type item)
    (define (put-helper k array)
        (cond 
            ((null? array) (list(make-entry k item)))
            ((equal? (key (car array)) k) array)
            (else (cons (car array) (put-helper k (cdr array))))))
    (set! global-array (put-helper (list op type) global-array)))

(define (get op type)
    (define (get-helper k array)
        (cond 
            ((null? array) #f)
            ((equal? (key (car array)) k) (value (car array)))
            (else (get-helper k (cdr array)))))
    (get-helper (list op type) global-array))

(define (attach-tag tag contents)
    (if (number? contents)
        contents
        (cons tag contents)))

(define (type-tag datum)
    (if (pair? datum)
        (car datum)
        (if (number? datum)
            'scheme-number
            (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
    (if (pair? datum)
        (cdr datum)
        (if (number? datum)
            datum
            (error "Bad tagged datum -- CONTENTS" datum))))

(define (apply-generic op . args)
    ;(display '!)
    (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
            (if proc 
                (apply proc (map contents args))
                (error 
                    "No method for these types -- APPLY-GENERIC"
                    (list op type-tags))))))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))

(define (install-polar-package)
    (define (magnitude z) (car z))
    (define (angle z) (cdr z))
    (define (make-from-mag-ang r a) (cons r a))
    (define (real-part z)
        (* (magnitude z) (cos (angle z))))
    (define (imag-part z)
        (* (magnitude z) (sin (angle z))))
    (define (make-from-real-imag x y)
        (cons (sqrt (+ (square x) (square y))) (atan y x)))

    (define (tag x) (attach-tag 'polar x))
    (put 'real-part '(polar) real-part)
    (put 'imag-part '(polar) imag-part)
    (put 'magnitude '(polar) magnitude)
    (put 'angle '(polar) angle)
    (put 'make-complex-from-real-imag 'polar
        (lambda (x y) (tag (make-complex-from-real-imag x y))))
    (put 'make-complex-from-mag-ang 'polar
        (lambda (x y) (tag (make-complex-from-mag-ang x y))))
    (put 'equ? '(polar polar)
        (lambda (z1 z2) (and (= (magnitude z1) (magnitude z2)) (= (angle z1) (angle z2)))))
    (put '=zero? '(polar) (lambda (z) (= (magnitude z) 0)))
    'done
)

(install-polar-package)

(define (gcd a b)
    (if (= a 0)
        b
        (gcd (remainder b a) a)))

(gcd 4 2)

(define (install-rectangular-package)
    (define (real-part z) (car z))
    (define (imag-part z) (cdr z))
    (define (make-from-real-imag x y) (cons x y))
    (define (magnitude z)
        (sqrt (+ (square (real-part z)) 
                 (square (imag-part z)))))
    (define (angle z) (atan (imag-part z) (real-part z)))
    (define (make-complex-from-mag-ang r a)
        (cons (* r (cos a)) (* r (sin a))))

    (define (tag x) (attach-tag 'rectangular x))
    (put 'real-part '(rectangular) real-part)
    (put 'imag-part '(rectangular) imag-part)
    (put 'magnitude '(rectangular) magnitude)
    (put 'angle '(rectangular) angle)
    (put 'make-complex-from-real-imag 'rectangular
        (lambda (x y) (tag (make-complex-from-real-imag x y))))
    (put 'make-complex-from-mag-ang 'rectangular
        (lambda (x y) (tag (make-complex-from-mag-ang x y))))
    (put 'equ? '(rectangular rectangular)
        (lambda (z1 z2) (and (= (real-part z1) (real-part z2)) (= (imag-part z1) (imag-part z2)))))
    (put '=zero? '(rectangular) (lambda (z) (and (= (real-part z)) (= (imag-part z) 0))))
    'done
)

(install-rectangular-package)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (install-scheme-number-package)
    (define (tag x)
        (attach-tag 'scheme-number x))
    (put 'add '(scheme-number scheme-number)
        (lambda (x y) (tag (+ x y))))
    (put 'sub '(scheme-number scheme-number)
        (lambda (x y) (tag (- x y))))
    (put 'mul '(scheme-number scheme-number)
        (lambda (x y) (tag (* x y))))
    (put 'div '(scheme-number scheme-number)
        (lambda (x y) (tag (/ x y))))
    (put 'make 'scheme-number
        (lambda (x) (tag x)))
    (put 'equ? '(scheme-number scheme-number) (lambda (x y) (= x y)))
    (put '=zero? '(scheme-number) (lambda (x) (= x 0)))
'done)

(define (make-scheme-number n)
    ((get 'make 'scheme-number) n))

(install-scheme-number-package)
(add (make-scheme-number 1) (make-scheme-number 2))

(define (install-rational-package)
    (define (numer x) (car x))
    (define (denom x) (cdr x))
    (define (make-rat n d)
        (let ((g (gcd n d)))
            (cons (/ n g) (/ d g))))
    (define (add-rat x y)
        (make-rat 
            (+ (* (numer x) (denom y)) 
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
    (define (sub-rat x y)
        (make-rat 
            (- (* (numer x) (denom y)) 
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
    (define (mul-rat x y)
        (make-rat 
            (* (numer x) (numer y))
            (* (denom x) (denom y))))
    (define (div-rat x y)
        (make-rat
            (* (numer x) (denom y))
            (* (denom x) (numer y))))

    (define (tag x) (attach-tag 'rational x))
    (put 'add '(rational rational)
        (lambda (x y) (tag (add-rat x y))))
    (put 'sub '(rational rational)
        (lambda (x y) (tag (sub-rat x y))))
    (put 'mul '(rational rational)
        (lambda (x y) (tag (mul-rat x y))))
    (put 'div '(rational rational)
        (lambda (x y) (tag (div-rat x y))))
    (put 'equ? '(rational rational)
        (lambda (x y) (and (= (numer x) (numer y)) (= (denom x) (denom y)))))
    (put '=zero? '(rational)
        (lambda (x) (not (= (numer x) 0))))
    (put 'make 'rational
        (lambda (n d) (tag (make-rat n d))))
    'done
 )

 (install-rational-package)

 (define (make-rational n d)
     ((get 'make 'rational) n d))

(define a (make-rational 2 4))
(define b (make-rational 1 2))
(equ? a b)

(define (install-complex-package)
    (define (make-from-real-imag x y)
        ((get 'make-from-real-imag 'rectangular) x y))
    (define (make-from-mag-ang r a)
        ((get 'make-from-mag-ang 'polar) r a))

    (define (add-complex z1 z2)
        (make-from-real-imag
            (+ (real-part z1) (real-part z2))
            (+ (imag-part z1) (imag-part z2))))
    (define (sub-complex z1 z2)
        (make-from-real-imag
            (- (real-part z1) (real-part z2))
            (- (imag-part z1) (imag-part z2))))

    (define (mul-complex z1 z2)
        (make-from-mag-ang
            (* (magnitude z1) (magnitude z2))
            (+ (angle z1) (angle z2))))
    (define (div-complex z1 z2)
        (make-from-mag-ang
            (/ (magnitude z1) (magnitude z2))
            (- (angle z1) (angle z2))))
    
    (define (tag z) (attach-tag 'complex z))

    (put 'make-from-real-imag 'rectangular 
        (lambda (x y) (attach-tag 'rectangular (cons x y))))
    (put 'make-from-mag-ang 'polar
        (lambda (r a) (attach-tag 'polar (cons r a))))

    (put 'add '(complex complex)
        (lambda (z1 z2) (tag (add-complex z1 z2))))
    (put 'sub '(complex complex)
        (lambda (z1 z2) (tag (sub-complex z1 z2))))
    (put 'mul '(complex complex)
        (lambda (z1 z2) (tag (mul-complex z1 z2))))
    (put 'div '(complex complex)
        (lambda (z1 z2) (tag (div-complex z1 z2))))
    (put 'make-from-real-imag 'complex
        (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'complex
        (lambda (r a) (tag (make-from-mag-ang r a))))

    (put 'magnitude '(complex) magnitude)

    (put 'equ? '(complex complex) equ?)
    (put '=zero? '(complex) =zero?)
'done)

(install-complex-package)

(define (make-complex-from-real-imag x y)
    ((get 'make-from-real-imag 'complex) x y))

(define k (get 'make-from-real-imag 'complex))
(k 1 2)

(define (make-complex-from-mag-ang r a)
    ((get 'make-from-mag-ang 'complex) r a))

(make-complex-from-real-imag 1 2)
(define a (make-complex-from-mag-ang 100 2))
(define b (make-complex-from-mag-ang 100 2))
(mul a b)
(magnitude a)
(add 1 2)
(equ? 4 4)
(equ? 3 4)
(equ? a b)
(=zero? 0)
(=zero? a)
(define c (make-complex-from-real-imag 0 0))
(=zero? c)