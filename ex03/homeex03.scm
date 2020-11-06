(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a)
          (accumulate op nv (next a) b term next))))

(define (id x) x)
(define (1+ x) (+ x 1))

(define (sum a b)
  (accumulate + 0 a b id 1+))

(define (2+ x) (+ x 2))

; Problem 1, can do lambda
(define (!! n)
  (accumulate * 1 (if (even? n) 2 1) n id 2+))

; Problem 2
(define (! n)
  (accumulate * 1 1 n id 1+))

(define (nchk n k)
  (/ (! n) (* (! k) (! (- n k)))))

; Problem 3
(define (nchk-oneacc n k)
  (define (nchk-el i)
    (/ (+ n (- 1 i)) i))
  (accumulate * 1 1 k nchk-el 1+))

; Problem 4.1
(define (2^ n)
  (accumulate * 1 1 n (lambda (x) 2) 1+))

; Problem 4.2
(define (2.1^ n)
  (accumulate + 0 0 n (lambda (x) (nchk n x)) 1+))

; Problem 5.1
(define (all? p? a b)
  (accumulate (lambda (x y) (and x y)) #t a b (lambda (x) (p? x))  1+))

(define (any? p? a b)
  (accumulate (lambda (x y) (or x y)) #f a b (lambda (x) (p? x))  1+))

; Problem 5.2
(define (filter-accumulate p? op nv a b term next)
  (cond ((> a b) nv)
        ((p? a) (op (term a)
                    (filter-accumulate p? op nv (next a) b term next)))
        (else       (filter-accumulate p? op nv (next a) b term next))))

; Problem 6
(define (divisors-sum n)
  (filter-accumulate (lambda (x) (= (remainder n x) 0)) + 0 1 n id 1+))

; Problem 7
(define (count p? a b)
  (filter-accumulate (lambda (x) (p? x)) + 0 a b (lambda (x) 1) 1+))

; Problem 8
(define (prime? n)
  (if (< n 2) #f
      (all? (lambda (x) (not (= (remainder n x) 0))) 2 (- n 1))))