(define key car)
(define left-tree cadr)
(define right-tree caddr)

(define (lookup given-key tree-of-records)
    (if (null? tree-of-records)
        false
        (cond 
            ((= given-key (key (car tree-of-records))) 
                (car tree-of-records))
            ((> given-key (key (car tree-of-records))) 
                (lookup given-key (right-tree tree-of-records)))
            ((< given-key (key (car tree-of-records))) 
                (lookup given-key (left-tree tree-of-records))))))
        
(define t (list (cons 2 200) (list (cons 1 100) '() '()) (list (cons 3 300) '() '())))
(key (car t))
(left-tree t)
(right-tree t)
(lookup 3 t)
(lookup 1 t)
(lookup 10 t)