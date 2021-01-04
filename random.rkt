(define (char-op? c)
  (and (char? c)
       (or
        (char=? c #\+)
        (char=? c #\-)
        (char=? c #\*)
        (char=? c #\/)
        (char=? c #\^)
        )
   )
)

(define (expr-valid? expr)

  (define (number i)
    (cond
      ((>= i (string-length expr))            #t)
      ((char-numeric?    (string-ref expr i)) (number (+ i 1)))
      ((char-op?         (string-ref expr i)) (op (+ i 1)))
      ((char-whitespace? (string-ref expr i)) (whitespace-after-number (+ i 1)))          
      (else                                   #f)
    )
  )
        
  (define (op i)
    (cond
      ((>= i (string-length expr))            #f)
      ((char-numeric?    (string-ref expr i)) (number (+ i 1)))
      ((char-whitespace? (string-ref expr i)) (whitespace-after-op (+ i 1)))          
      (else                                   #f)
    )
  )

  (define (whitespace-after-number i)
    (cond
      ((>= i (string-length expr))            #t)
      ((char-op?         (string-ref expr i)) (op (+ i 1)))
      ((char-whitespace? (string-ref expr i)) (whitespace-after-number (+ i 1)))
      (else                                   #f)
    )
  )

  (define (whitespace-after-op i)
    (cond
      ((>= i (string-length expr))            #f)
      ((char-numeric?    (string-ref expr i)) (number (+ i 1)))
      ((char-whitespace? (string-ref expr i)) (whitespace-after-op (+ i 1)))
      (else                                   #f)
    )
  )

  (define (whitespace-at-start i)
    (cond
      ((>= i (string-length expr))            #t)
      ((char-numeric?    (string-ref expr i)) (number (+ i 1)))
      ((char-whitespace? (string-ref expr i)) (whitespace-at-start (+ i 1)))
      (else                                   #f)
    )
  )

  (cond
    ((= 0 (string-length expr))          #t)
    ((char-numeric? (string-ref expr 0)) (number 0))
    (else                                (whitespace-at-start 0))    
  )
)