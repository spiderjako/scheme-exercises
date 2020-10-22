(define (sum a b)
  (define (sum-helper start sum)
    (if (> start b) sum
        (sum-helper (+ 1 start) (+ sum start))))
  (sum-helper a 0))

(define (fast-sq x n)
  (define (sq x) (* x x))
  (cond ((= n 0) 1)
        ((even? n) (sq (fast-sq x (quotient n 2))))
        (else (* x (sq (fast-sq x (quotient n 2)))))))

(define (last-digit n)
  (remainder n 10))

(define (count-digit d n)
  (define (helper newN count)
    (cond ((= newN 0) count)
          ((= d (last-digit n)) (helper (quotient newN 10) (+ 1 count)))
          (else (helper (quotient newN 10) count))))
  (if (= n 0) (if (= d 0) 1 0) (helper n 0)))

(define (reverse-int n)
  (define (reverse old new)
    (if(= old 0) new
       (reverse (quotient old 10) (+ (remainder old 10) (* new 10)))))
  (reverse n 0))

(define (palindrome? n)
  (= n (reverse-int n)))

(define (divisors-sum n)
  (define (helper divisor sum)
    (if (= divisor n) sum
        (if (= (remainder n divisor) 0) (helper (+ 1 divisor) (+ divisor sum))
            (helper (+ 1 divisor) sum))))
  (helper 1 0))

(define (perfect? n)
  (= (divisors-sum n) n))

(define (prime? n)
  (if (> n 1) (= (divisors-sum n) 1) #f))

(define (increasing? n)
  (if (= (quotient n 10) 0) #t
    (if (> (last-digit n) (last-digit (quotient n 10))) (increasing? (quotient n 10))
        #f)))

(define (toBinary n)
  (define (helper newN sum)
    (if (= newN 1) (+ (* sum 10) 1)
        (if (= (remainder newN 2) 1) (helper (quotient newN 2) (+ (* sum 10) 1))
            (helper (quotient newN 2) (* sum 10)))))
  (reverse-int (helper n 0)))