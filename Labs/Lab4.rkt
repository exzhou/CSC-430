#lang racket
(require rackunit)

; 2. Lambdas
((lambda (x) (+ x 2)) 3)

((lambda (f g) (f (g 3)))
 (lambda (x) (+ x 3))
 (lambda (x) (* x 2)))



; 3. curried-add
(define (curried-add a)
  (lambda (b) (+ a b)))
(check-equal? ((curried-add 1) 3) 4)



; 4. Curry2
(define (curry2 f)
  (lambda (a) (lambda (b) (f a b))))
(check-equal? (((curry2 (lambda (a b) (+ a b))) 1) 2) 3)



; 5. Curry3
(define (curry3 f)
  (lambda (a) (lambda (b) (lambda (c) (f a b c)))))
(check-equal? ((((curry3 (lambda (a b c) (+ a b c))) 1) 2) 3) 6)



;6. Contains
(define (contains? lst sym)
  (cond
    [(empty? lst) #f]
    [(cond
       [(and (equal? (length lst) 1) (equal? lst sym)) #t]
       [else (contains? (rest lst) sym)])]))
(check-true (contains? `s `(a b c s)))
(check-false (contains? `s `(a b c)))



;7 in-list-many?
(define (in-list-many? src qry)
  (map ((curry2 contains?) src) qry))

  

;(check-equal? (in-list-many? '(a b c d) '(f a a c)) '(#f #t #t #t))
;(check-equal? (in-list-many? '(a b c) '(d e a)) '(#f #f #t))
