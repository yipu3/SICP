(load "2_62.scm")
(load "2_63.scm")
(load "2_64.scm")

(define (union-set-tree s1 s2)
    (let 
        ((l1 (tree->list-2 s1))
        (l2 (tree->list-2 s2)))
        (list->tree (union-set l1 l2))))

(define (intersection-set-tree s1 s2)
    (let 
        ((l1 (tree->list-2 s1))
        (l2 (tree->list-2 s2)))
        (list->tree (intersection-set l1 l2))))

(define t1 (list->tree (list 2 4 6 9 11)))
(define t2 (list->tree (list 1 4 5 7 11)))

(union-set-tree t1 t2)
(intersection-set-tree t1 t2)