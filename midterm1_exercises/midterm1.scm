(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a)
          (accumulate op nv (next a) b term next))))

(define (filter-accumulate p? op nv a b term next)
  (if (> a b) nv
      (if (p? (term a)) (op (term a)
                            (filter-accumulate p? op nv (next a) b term next))
          (filter-accumulate p? op nv (next a) b term next))))

(define (1+ x) (+ x 1))

(define (factorial n)
  (if (< n 2) 1
      (* n (factorial (- n 1)))))

(define (sum-digits a)
  (define (helper currNumber sum)
    (if (= currNumber 0) sum
        (helper (quotient currNumber 10) (+ sum (remainder currNumber 10)))))
  (helper a 0))

(define (contains-two-same-digits? number)
  (define (contains-another-such-digit? number a)
    (if (= number 0) #f
        (if (= (remainder number 10) a) #t
            (contains-another-such-digit? (quotient number 10) a))))
  (cond ((= number 0) #f)
        ((contains-another-such-digit? (quotient number 10) (remainder number 10)) #t)
        (else (contains-two-same-digits? (quotient number 10)))))

;; (define (strange-compare a b))

(define (toBinary n)
  (if (= n 0) 0
      (+ (remainder n 2) (* 10 (toBinary (quotient n 2))))))

(define (count-ones-in-binary a)
  (define (helper b count)
    (if (= b 0) count
        (if (= (remainder b 10) 1) (helper (quotient b 10) (1+ count))
            (helper (quotient b 10) count))))
  (helper (toBinary a) 0))

(define (exponent-power n)
  (define (id i) (/ (expt n i) (factorial i)))
  (accumulate + 1 1 n id 1+))

(define (strange-sum a b)
  (define (p? x) (= (remainder (sum-digits x) 2) 1))
  (filter-accumulate p? + 0 a b (lambda (x) x) 1+))