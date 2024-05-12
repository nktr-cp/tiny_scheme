10
(+ (- (* (* 1 2) 3) (* 4 5)) (+ (* 6 7) (* 8 9)))

(define a 100)
(define b (+ 10 1))

(if (< a b) 10 20)
(if (> a b) 10 20)
(if (= a 10) "Yes" "No")

(quote (+ 1 2))

(define f (lambda (x) (+ x 1)))
(f 10)

(define g (lambda (x y) (+ x y)))
(g 3 5)

(define h (lambda (x) (lambda (y) (+ x (* a y)))))
((h 9) 9)

(define i (lambda (x) (if (< x a) a x)))
(i (f 100))

(let ((a 1)) (i 10))
(let ((a 10) (b (f a)) (c (- b 1))) (let ((a 10) (b (f a)) (c (- b 1))) (+ (+ a b) c)))

(define l (cons 1 (cons 2 (cons 3 (list)))))
(car (cdr l))

(define fact (lambda (n) (if (< n 1) 1 (* n (fact (- n 1))))))
(fact 10)

(define (add x y) (+ x y))
(add 5 3)
(let ((a 20))
  (add a 5))

(define lst (list 1 2 3 4 5))
(car lst)
(cdr lst)

