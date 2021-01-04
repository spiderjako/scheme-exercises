(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define empty? null?)

(define head car)
(define tail cdr)

(define (identical-trees? t1 t2)
  (cond ((and (empty? t1) (empty? t2)) #t)
        ((or (and (empty? t1) (not (empty? t2))) (and (not (empty? t1)) (empty? t2))) #f)
        (else (and (= (root-tree t1) (root-tree t2)) (identical-trees? (left-tree t1) (left-tree t2)) (identical-trees? (right-tree t1) (right-tree t2))))))

(define (tree-sum t)
  (if (empty? t) 0
      (+ (root-tree t) (+ (tree-sum (left-tree t)) (tree-sum (right-tree t))))))

(define (change-tree t)
  (if (empty? t) '()
      (list (tree-sum t) (change-tree (left-tree t)) (change-tree (right-tree t)))))

(define (path-sum-of-elems? number t)
  (define (helper currSum currT)
    (cond ((empty? currT) #f)
          ((= number (+ (root-tree currT) currSum)) #t)
          (else (or (helper (+ currSum (root-tree currT)) (left-tree currT)) (helper (+ currSum (root-tree currT)) (right-tree currT))))))
  (helper 0 t))


(define (level k t)
  (cond ((empty? t) '())
        ((= k 0) (list (root-tree t)))
        (else (append (level (- k 1) (left-tree t)) (level (- k 1) (right-tree t))))))

(define (height t)
  (define (helper h currtree)
    (if (empty? currtree) h
        (max (helper (+ 1 h) (left-tree currtree)) (helper (+ 1 h) (right-tree currtree)))))
  (helper 0 t))

(define (foldr op nv lst)
  (if (empty? lst) nv
      (op (head lst)
          (foldr op nv (tail lst)))))

(define (foldl op nv lst)
  (if (empty? lst) nv
      (foldl op (op nv (head lst)) (tail lst))))


(define (all-levels number t)
  (define (helper i h t)
    (cond ((= i h) #f)
          ((= number (foldr + 0 (level i t))) #t)
          (else (or (helper (+ 1 i) h t) (helper (+ 1 i) h t)))))
  (helper 0 (height t) t))

(define (find-elem v1 v2 t)
  (define (helper x currtree)
    (cond ((empty? currtree) x)
          ((and (and (not (empty? (left-tree currtree)))  (= (head (left-tree currtree)) v1))
                (and (not (empty? (right-tree currtree))) (= (head (right-tree currtree)) v2))) (min (helper (head currtree) (left-tree currtree))
                                                                                                     (helper (head currtree) (right-tree currtree))))
          (else (min (helper x (left-tree currtree))
                     (helper x (right-tree currtree))))))
  (helper 0 t))

(define (bst-insert t val)
  (cond ((empty? t) (list val '() '()))
        ((< val (root-tree t)) (list (root-tree t) (bst-insert (left-tree t) val ) (right-tree t)))
        (else (list (root-tree t) (left-tree t) (bst-insert (right-tree t) val)))))

(define (make-bst lst)
  (foldl bst-insert '() lst))