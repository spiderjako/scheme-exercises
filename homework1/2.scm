(define (toBinary n)
  (if (= n 0) 0
      (+ (remainder n 2) (* 10 (toBinary (quotient n 2))))))

(define (toDecimal binary)
  (define (helper n decimal power)
    (if (= n 0) decimal
        (helper (quotient n 10) (+ (* (remainder n 2) (expt 2 power)) decimal) (+ power 1))))
  (helper binary 0 0))

(define (elem-in-set elem)
  (toBinary (expt 2 elem)))

(define (set-contains? set elem)
  (= (remainder (quotient (toBinary set) (elem-in-set elem)) 2) 1))

(define (set-add set elem)
  (if (set-contains? set elem) set
      (toDecimal (+ (toBinary set) (elem-in-set elem)))))

(define (set-remove set elem)
  (if (set-contains? set elem) (toDecimal (- (toBinary set) (elem-in-set elem)))
      set))

(define (set-empty? set)
  (= set 0))

(define (set-size set)
  (define (helper newSet len)
    (if (= newSet 0) len
        (if (= (remainder newSet 2) 1) (helper (quotient newSet 10) (+ 1 len))
            (helper (quotient newSet 10) len))))
  (helper (toBinary set) 0))
          

(define (set-intersect set1 set2)
  (define (helper s1 s2 intSet pos)
    (if (or (= s1 0) (= s2 0)) intSet
        (if (and (= (remainder s1 2) 1) (= (remainder s2 2) 1)) (helper (quotient s1 10) (quotient s2 10) (+ intSet (expt 10 pos)) (+ 1 pos))
            (helper (quotient s1 10) (quotient s2 10) intSet (+ 1 pos)))))
  (toDecimal (helper (toBinary set1) (toBinary set2) 0 0)))

(define (set-union set1 set2)
  (define (helper s1 s2 uniSet pos)
    (if (and (= s1 0) (= s2 0)) uniSet
        (if (or (= (remainder s1 2) 1) (= (remainder s2 2) 1)) (helper (quotient s1 10) (quotient s2 10) (+ uniSet (expt 10 pos)) (+ 1 pos))
            (helper (quotient s1 10) (quotient s2 10) uniSet (+ 1 pos)))))
  (toDecimal (helper (toBinary set1) (toBinary set2) 0 0)))


(define (set-difference set1 set2)
  (define (helper s1 s2 diffSet pos)
    (if (and (= s1 0) (= s2 0)) diffSet
        (if (and (= (remainder s1 2) 1) (= (remainder s2 2) 0)) (helper (quotient s1 10) (quotient s2 10) (+ diffSet (expt 10 pos)) (+ 1 pos))
            (helper (quotient s1 10) (quotient s2 10) diffSet (+ 1 pos)))))
  (toDecimal (helper (toBinary set1) (toBinary set2) 0 0)))

(define (max? a b)
  (if (> a b) a b))

(define (knapsack c n w p)
  (define (calc-set-of-items set c n w p)
    (if (or (= c 0) (= n 0)) set
        (if (> (w (- n 1)) c) (calc-set-of-items set c (- n 1) w p)
            (max? (calc-set-of-items (set-union set (toDecimal (expt 10 (- n 1)))) (- c (w (- n 1))) (- n 1) w p)
                  (calc-set-of-items set c (- n 1) w p)))))
  (calc-set-of-items 0 c n w p))