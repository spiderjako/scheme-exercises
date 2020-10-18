(define (custom-sum a b sum)
  (if (= a b) (+ a sum)
      (custom-sum (+ a 1) b (+ sum a))))

(define (fast-sq x n)
  (define (sq x) (* x x))
  (define (power x n prod)
    (if (< n 1) prod
        (power x (- n 1) (* x prod))))
  (if (= 0 (remainder n 2)) (sq (power x (/ n 2) 1))
      (* x (sq (power x (/ n 2) 1)))))

(define (count-digit d n)
  (define (func-help a b x)
    (if(= b 0) x
       (if(= (remainder b 10) a) (func-help a (quotient b 10) (+ x 1))
           (func-help a (quotient b 10) x))))
  (func-help d n 0))

(define (reverse-int n)
  (define (reverse old new)
    (if(= old 0) new
       (reverse (quotient old 10) (+ (remainder old 10) (* new 10)))))
  (reverse n 0))

(define (palindrome? n)
  (define (length n x)
    (if (= n 0) x
        (length (quotient n 10) (+ x 1))))
  (define (power x n prod)
    (if (= n 0) x
        (power x (- n 1) (* x prod))))
  (define (palindrome-helper n)
    (= (quotient n (power 10 (- (length n 0) 1) 1)) (remainder n 10)))
  (palindrome-helper n))
    