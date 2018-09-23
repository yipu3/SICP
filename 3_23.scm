(define (make-deque)
    (cons '() '()))

(define (front-deque queue)
    (car queue))

(define (rear-deque queue)
    (cdr queue))

(define (empty-deque? queue)
    (null? (front-ptr queue)))

(define (set-front-ptr! queue item)
    (set-car! queue item))

(define (set-rear-ptr! queue item)
    (set-cdr! queue item))

(define (front-queue queue)
    (if (empty-queue? queue)
        (error "FRONT called with an empty queue" queue)
        (car (front-ptr queue))))

(define (front-insert-queue! queue item)
    (let ((new-pair (cons item '())))
        (cond ((empty-queue? queue)
            (set-front-ptr! queue new-pair)
            (set-rear-ptr! queue new-pair)
            queue)
        (else
            (set-cdr! (rear-ptr queue) new-pair)
            (set-rear-ptr! queue new-pair)
            queue))))

(define (delete-queue! queue)
    (cond ((empty-queue? queue)
        (error "DELETE! called with an empty queue" queue))
        (else
            (set-front-ptr! queue (cdr (front-ptr queue)))
            queue)))