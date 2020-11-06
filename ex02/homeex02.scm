(define (constantly c)
  (lambda (x) c))

(define (flip f)
  (lambda (x y) (f y x)))

(define (complement p)
  (lambda (x) (not (p x))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define id (lambda (x) x))

(define (repeat n f)
  (if (= n 0) id
      (compose f (repeat (- n 1) f))))

(define dx 0.000001)

(define (derive f)
  (lambda (x) (/ (- (f (+ x dx)) (f x)) dx)))

(define (derive-n n f)
  (if (= n 0) f
      (derive (derive-n (- n 1) (derive f)))))

(define (twist k f g)
  (if (= k 0) id
      (lambda (x) (f (g ((twist (- k 2) f g) x))))))