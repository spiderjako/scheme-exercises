(define (1- x) (- x 1))
(define (1+ x) (+ x 1))

(define empty? null?)
(define head car)
(define tail cdr)

(define (take n lst)
  (if (or (= n 0) (empty? lst)) `()
      (cons (head lst) (take (1- n) (tail lst)))))

(define (all? p? lst)
  (if (empty? lst) #t
      (and (p? (head lst))
           (all? p? (tail lst)))))

(define (any? p? lst)
  (if (empty? lst) #f
      (or (p? (head lst))
          (any? p? (tail lst)))))

(define (zip lst1 lst2)
  (if (or (empty? lst1) (empty? lst2)) `()
      (cons (cons (head lst1) (head lst2)) (zip (tail lst1) (tail lst2)))))

(define (zipWith f lst1 lst2)
  (if (or (empty? lst1) (empty? lst2)) `()
      (cons (f (head lst1) (head lst2)) (zipWith f (tail lst1) (tail lst2)))))

(define (zip* lst1 lst2)
  (zipWith cons lst1 lst2))

(define (map* f lst)
  (if (empty? lst) `()
      (cons (f (head lst)) (map f (tail lst)))))

(define (filter* p? lst)
  (cond ((empty? lst) `())
        ((p? (head lst)) (cons (head lst) (filter* p? (tail lst))))
        (else (filter* p? (tail lst)))))

(define (sorted? lst)
  (or (empty? lst)
      (empty? (tail lst))
      (and (<= (head lst) (head(tail lst))) (sorted? (tail lst)))))

(define (uniques lst)
  (cond ((empty? lst) `())
        (else (cons (head lst) (uniques (filter* (lambda (x) (not (equal? x (head lst)))) (tail lst)))))))

(define (foldr f nv lst)
  (if (empty? lst) nv
      (f (head lst)
         (foldr f nv (tail lst)))))

(define (insert val lst)
  (cond ((empty? lst) (list val))
        ((> val (head lst)) (cons (head lst) (insert val (tail lst))))
        (else (cons val lst))))

(define (insertion-sort lst)
  (foldr insert `() lst))

(define (is-subinterval? i1 i2)
  (and (>= (head i1) (head i2)) (<= (tail i1) (tail i2))))


(define (interval-length iv)
  (- (tail iv) (head iv)))

(define (longest-interval il)
  (define (helper currMax currL)
    (cond ((empty? currL) currMax)
          ((> (interval-length (head currL)) (interval-length currMax)) (helper (head currL) (tail currL)))
          (else (helper currMax (tail currL)))))
  (helper (head il) il))

(define (longest-interval-subsets il)
  (define maxint (longest-interval il))
  (define (insert-interval int il)
    (cond ((empty? il) (list int))
          ((> (head int) (head (head il))) (cons (head il) (insert-interval int (tail il))))
          (else (cons int il))))
  
  (foldr insert-interval `() (filter* (lambda (x) (is-subinterval? x maxint)) il)))


(define (compose f g) (lambda (x) (f (g x))))
(define (compose* . fns)
  (foldr compose (lambda (x) x) fns))

(define (group-by f lst)
  (if (empty? lst) `()
      (cons (list (f (head lst))
                  (filter* (lambda (x) (equal? (f x) (f (head lst)))) lst))
            (group-by f (filter* (lambda (x) (not (equal? (f x) (f (head lst))))) (tail lst))))))


(define (zipWith* f . lsts)
  (if (or (empty? lsts) (any? empty? lsts)) `()
      (cons (apply f (map head lsts))
            (apply zipWith* f (map tail lsts)))))