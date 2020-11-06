(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a)
          (accumulate op nv (next a) b term next))))

(define (1+ x) (+ x 1))

(define (upper-line m)
  (define (id-helper x)
    (cond ((= x 1) (string #\u250C))
          ((= x m) (string #\u2510))
          (else (string #\u2500))))
  (accumulate string-append "" 1 m id-helper 1+))

(define (lower-line m)
  (define (id-helper x)
    (cond ((= x 1) (string #\u2514))
          ((= x m) (string #\u2518))
          (else (string #\u2500))))
  (accumulate string-append "" 1 m id-helper 1+))

(define (left-walls m)
  (accumulate string-append "" 0 (- m 1) (lambda (x) (string-append (string #\u2502) " ")) 1+))

(define (right-walls m)
  (accumulate string-append "" 0 (- m 1) (lambda (x) (string-append " " (string #\u2502))) 1+))

(define (squares n)
  ;; The lower the level, the less the '-'s on the upper line
  (define (upper-formula n x)
    (- (+ (* 2 (- (* n 2) 2)) 3)
       (* x 4)))

  ;; The lower the level, the more the '-'s on the lower line
  (define (lower-formula n x)
    (- (+ (* 2 (- (* n 2) 2)) 3)
       (* (- n (1+ x)) 4)))
  
  (define (upper-helper)
    (define (helper-id x)
      (string-append (string-append (string-append (left-walls (- n (- n x)))
                                                   (upper-line (upper-formula n x)))
                                    (right-walls (- n (- n x))))
                     "\n"))
    (accumulate string-append "" 0 (- n 1) helper-id 1+))

  (define (lower-helper)
    (define (helper-id x)
      (string-append (string-append (string-append (left-walls (- n (1+ x)))
                                                   (lower-line (lower-formula n x)))
                                    (right-walls (- n (1+ x))))
                     "\n"))
    (accumulate string-append "" 0 (- n 1) helper-id 1+))
  (display (upper-helper))
  (display (lower-helper)))