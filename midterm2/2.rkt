(define head car)
(define tail cdr)

(define (dropWhile p? lst)
  (cond ((null? lst) `())
        ((p? (head lst)) (dropWhile p? (tail lst)))
        (else lst)))

(define (takeWhile p? lst)
  (cond ((null? lst) `())
        ((p? (head lst)) (cons (head lst) (takeWhile p? (tail lst))))
        (else `())))

;; Basic matrix operations
(define (number-rows m) (length m))
(define (number-columns m)
  (if (null? m) 0
      (length (head m))))

;; Row operations
(define (first-row m) (head m))
(define (remove-first-row m) (tail m))
(define (get-row m n) (list-ref m n))

;; Column operations
(define (first-col m) (map head m))
(define (remove-first-col m) (map tail m))
(define (get-column m n)
  (map (lambda (x) (list-ref x n)) m))

;; Fetch element
(define (get-element m i j)
  (list-ref (list-ref m j) i))

;; Remove element from list
(define (remove-element lst n)
  (cond ((null? lst) '())
        ((= n 0) (tail lst))
        (else (cons (head lst) (remove-element (tail lst) (- n 1))))))

;; Remove rows or columns
(define (remove-row M n)
  (remove-nth M n))

(define (remove-column M n)
  (map (lambda(x) (remove-nth x n)) M))

;; Transpose
;; map can be used on more that two arguments
(define (transpose M)
  (apply map list M))

(define m '((1 2 3 4 5 6)
            (2 3 4 5 8 9)
            (0 2 6 4 8 2)
            (5 2 8 4 9 0)))

(define (sub-range i j lst)
  (drop (take lst j) i))

(define (sub-matrix i1 j1 i2 j2 m)
  (let ((the-rows (sub-range i1 i2 m)))
    (map (lambda (x) (sub-range j1 j2 x)) the-rows)))


(define (foldr op nv lst)
  (if (null? lst) nv
      (op (head lst)
          (foldr op nv (tail lst)))))

(define (foldr-matrix op-rows nv-rows op-elems nv-elems m)
  (foldr op-elems nv-elems (map (lambda (x) (foldr op-rows nv-rows x)) m)))

;; Binary Trees

(define empty? null?)
(define empty-tree '())
(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)

(define (tree? t)
  (or (empty? t)
      (and (list? t) (= (length t) 3) (tree? (left-tree t)) (tree? (right-tree t)))))

(define (tree-sum t)
  (if (empty? t) 0
      (+ (root-tree t) (+ (tree-sum (left-tree t)) (tree-sum (right-tree t))))))

(define (tree-level k t)
  (cond ((empty? t) '())
        ((and (= k 0) (not (empty? t))) (list (head t)))
        (else (append (tree-level (- k 1) (left-tree t)) (tree-level (- k 1) (right-tree t))))))

(define (height t)
  (if (empty? t) 0
      (max (+ 1 (height (left-tree t))) (+ 1 (height (right-tree t))))))

(define (all-levels t)
  (define (helper i h t)
    (if (or (empty? t) (= i h)) '()
        (cons (tree-level i t) (helper (+ i 1) h t))))
  (helper 0 (height t) t))

(define (tree-map f t)
  (if (empty? t) t
      (list (f (head t)) (tree-map f (left-tree t)) (tree-map f (right-tree t)))))

(define (tree->list t)
  (if (empty? t) '()
      (append (tree->list (left-tree t)) (list (root-tree t)) (tree->list (right-tree t)))))

(define (bst-insert val t)
  (cond ((empty? t) (list val '() '()))
        ((< val (head t)) (list (head t) (bst-insert val (left-tree t)) (right-tree t)))
        (else (list (head t) (left-tree t) (bst-insert val (right-tree t))))))

(define (tree-sort t)
  (tree->list (foldr bst-insert '() t)))

(define (valid-bst? t)
  (if (empty? t) #t
      (and (or (empty? (left-tree t)) (< (head (left-tree t)) (root-tree t)))
           (or (empty? (right-tree t))(> (head (right-tree t)) (root-tree t)))
           (valid-bst? (left-tree t))
           (valid-bst? (right-tree t)))))

(define (is-leaf? t)
  (and (empty? (left-tree t)) (empty? (right-tree t))))

(define (prune t)
  (if (is-leaf? t) '()
      (list (head t) (prune (left-tree t)) (prune (right-tree t)))))

(define (bloom t)
  (cond ((empty? t) '())
        ((is-leaf? t) (list (root-tree t) (list (root-tree t) '() '()) (list (root-tree t) '() '())))
        (else (list (root-tree t) (bloom (left-tree t)) (bloom (right-tree t))))))

(define (minimal t)
  (if (is-leaf? t) (root-tree t)
      (min (minimal (left-tree t)) (minimal (right-tree t)))))

(define (maximal t)
  (if (is-leaf? t) (root-tree t)
      (max (maximal (left-tree t)) (maximal (right-tree t)))))

(define (avg t)
  (let ((x (maximal t))
        (y (minimal t)))
    (if (= y 0) 0
        (/ x y))))

(define (same-as-code t)
  (define (helper bn ct)
    (cond ((empty? ct) '())
          ((= bn (root-tree ct)) (append (list (root-tree ct)) (append (helper (* 10 bn) (left-tree ct)) (helper (+ (* 10 bn) 1) (right-tree ct)))))
          (else (append (helper 0 (left-tree ct)) (helper 1 (right-tree ct))))))
  (helper 1 t))