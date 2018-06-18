(define (enumerate-interval low high)
    (if (>= low high)
        (list low)
        (cons low (enumerate-interval (+ low 1) high))))

(enumerate-interval 2 7)


(define (flatmap proc seq)
    (accumulate append '() (map proc seq)))

(define (accumulate proc init seq)
    (if (null? seq)
        init
        (proc (car seq) (accumulate proc init (cdr seq)))))

(accumulate + 0 (list 1 2 3 4))

(define (cons-every n)
    (map (lambda (j) (list n j)) 
            (enumerate-interval 1 (- n 1))))

(cons-every 10)

(define (unique-pairs n)
    (flatmap cons-every (enumerate-interval 1 n)))

(map cons-every (enumerate-interval 1 2))

(enumerate-interval 1 2)
(unique-pairs 2)

(define (make-pair-sum pair)
    (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (filter judge seq)
    (if (null? seq)
        ()
        (if (judge (car seq))
            (cons (car seq) (filter judge (cdr seq)))
            (filter judge (cdr seq)))))

(define (even? x)
    (= (remainder x 2) 1))

(filter even? (list 1 2 3 4))

(define (prime? x)
  (define (prime-helper x k)
    (cond ((= x k) #t)
          ((= (remainder x k) 0) #f)
          (else
           (prime-helper x (+ k 1)))))
  (cond ((= x 1) #f)
        ((= x 2) #t)
         (else
          (prime-helper x 2))))

(define (prime-sum? pair)
    (prime? (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
    (map make-pair-sum
        (filter prime-sum? (unique-pairs n))))

(unique-pairs 10)

(prime-sum-pairs 10)

(define (not-contain j)
    (lambda (listx) 
        (not (or (= (car listx) j) (= (cadr listx) j)))))

(define (not-equal listx)
    (not (= (car listx) (cadr listx))))

((not-contain 10) (list 1 10))

(define (reverse listx)
    (list (cadr listx) (car listx)))

(define (unique-3 n)
    (flatmap (lambda (i) (map (lambda (j) (append j (list i))) (filter not-equal 
        (filter (not-contain i) (map reverse (unique-pairs (- i 1)))))))
        (enumerate-interval 1 n)))

(unique-3 10)

(define (judge-3 x)
    (= (+ (car x) (cadr x)) (caddr x)))

(judge-3 (list 1 2 3))

(filter judge-3 (unique-3 10))