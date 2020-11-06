(define (1+ x) (+ x 1))
(define (1- x) (- x 1))

(define (substring? substr str)
  (define (helper i)
    (if (> (+ i (string-length substr)) (string-length str)) #f
        (if (string=? (substring str i (+ i (string-length substr))) substr) #t
            (helper (1+ i)))))
  (helper 0))

(define (filter-accumulate p? op nv a b term next)
  (cond ((> a b) nv)
        ((p? a) (op (term a)
                    (filter-accumulate p? op nv (next a) b term next)))
        (else       (filter-accumulate p? op nv (next a) b term next))))

(define (cut-useless-spaces expr)
  (define (is-not-whitespace i)
    (not (substring? (substring expr i (1+ i)) " ")))
  (define (str-id i)
    (substring expr i (1+ i)))
  (filter-accumulate is-not-whitespace string-append "" 0 (- (string-length expr) 1) str-id 1+))

(define (string-is-operator? str i)
  (substring? (substring str i (1+ i)) "+-*/^"))

(define (string-is-number? str i)
  (substring? (substring str i (1+ i)) "0123456789"))

(define (string-is-whitespace? str i)
  (if (> i (1- (string-length str))) #f
      (substring? (substring str i (1+ i)) " ")))
  
(define (neighbouring-chars-are-numbers? expr i)
  (if (or (= i 0) (= i (- (string-length expr) 1))) #f
      (and  (string-is-number? expr (1- i)) (string-is-number? expr (1+ i)))))

(define (two-number-neighbours? expr)
  (define (helper i shouldBeNumber)
    (if (> i (1- (string-length expr))) #t
        (if shouldBeNumber (cond ((string-is-whitespace? expr i) (helper (1+ i) #t))
                                 ((string-is-operator? expr i) (helper (1+ i) #f))
                                 (else #f))
            (if (and (string-is-number? expr i) (string-is-whitespace? expr (1+ i))) (helper (1+ i) #t) (helper (1+ i) #f)))))
  (helper 1 #f))

(define (expr-valid? expr)
  (define (parse-string newExpr i)
    (if (> i (1- (string-length newExpr))) #t
        (if (string-is-operator? newExpr i) (and (neighbouring-chars-are-numbers? newExpr i) (parse-string newExpr (1+ i)))
            (parse-string newExpr (1+ i)))))
  (and (two-number-neighbours? expr)(parse-string (cut-useless-spaces expr) 0)))


(define (expr-eval expr)
  (define (eval-op str a b)
    (cond ((substring? str "+") (+ a b))
          ((substring? str "-") (- a b))
          ((substring? str "*") (* a b))
          ((substring? str "/") (/ a b))
          (else (expt a b))))

  ;; Left associativity makes the tree branch out in only the left direction, when starting from the end of the expression
  ;; Too late for corrections, turned out that priority and associativity aren't the same thing, hence the error prone example: 10+2*3
  (define (helper validExpression currNumber i power)
    (if (< i 0) currNumber
        (if (string-is-number? validExpression i) (helper validExpression (+ currNumber (* (expt 10 power) (string->number (substring validExpression i (1+ i))))) (1- i) (1+ power))
            (eval-op (substring validExpression i (1+ i)) (helper validExpression 0 (1- i) 0) currNumber))))
  (define spacelessExpression (cut-useless-spaces expr))
  (if (expr-valid? expr) (helper spacelessExpression 0 (1- (string-length spacelessExpression)) 0) #f))
  