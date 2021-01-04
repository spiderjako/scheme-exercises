(define (tree? t)
  (or (null? t)
      (and (list? t)
           (= (length t) 3))
      (tree? (cadr t))
      (tree? (caddr t))))

(define empty-tree '())

(define (make-tree root left right) (list root left right))
(define (make-leaf root) (make-tree root empty-tree empty-tree))

(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define empty-tree? null?)

(define test-tree
  (make-tree 10
             (make-tree 7
                        (make-leaf 10)
                        (make-leaf 2))
             (make-tree 3
                        (make-tree 4
                                   (make-leaf 1)
                                   (make-leaf 2))
                        empty-tree)))

(define (tree-sum t)
  (if (empty-tree? t) 0
      (+ (root-tree t) (+ (tree-sum (left-tree t)) (tree-sum (right-tree t))))))

(define (tree-level k t)
  (cond ((empty-tree? t) '())
        ((= k 0) (list (root-tree t)))
        (else (append (tree-level (- k 1) (left-tree t)) (tree-level (- k 1) (right-tree t))))))

(define (all-levels t)
  (define (helper count curr-tree)
    (if (empty-tree? curr-tree) count
        (max (helper (+ 1 count) (left-tree curr-tree)) (helper (+ 1 count) (right-tree curr-tree)))))
  (helper 0 t))

(define (tree-map f t)
  (if (empty-tree? t) t
      (make-tree (f (root-tree t)) (tree-map f (left-tree t))(tree-map f (right-tree t)))))

(define (tree->list t)
  (if (empty-tree? t) '()
      (append (tree->list (left-tree t)) (list (root-tree t)) (tree->list (right-tree t)))))

(define (bst-insert val t)
  (cond ((empty-tree? t) (make-leaf val))
        ((< val (root-tree t)) (make-tree (root-tree t) (bst-insert val (left-tree t)) (right-tree t)))
        ((> val (root-tree t)) (make-tree (root-tree t) (left-tree t) (bst-insert val (right-tree t))))
        (else t)))

(define (tree-sort lst)