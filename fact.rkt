(define (fact-old n)
  (define (fact-helper n)
    (if (= n 1) 1
        (* n (fact-helper(- n 1)))))
  (fact-helper n)
)


(define (fact n)
  (define (loop i result)
    (if (> i n) result
        (loop (+ i 1) (* i result))))
  (loop 1 1))
