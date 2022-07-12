#lang typed/racket
(require typed/rackunit)


(define-type ExprC (U NumC IdC StringC LamC AppC ifC))
(struct NumC ([n : Real]) #:transparent)
(struct IdC ([s : Symbol]) #:transparent)
(struct StringC ([s : String]) #:transparent)
(struct LamC ([args : (Listof Symbol)] [body : ExprC]) #:transparent)
(struct AppC ([fun : ExprC] [args : (Listof ExprC)]) #:transparent)
(struct ifC ([if : ExprC] [then : ExprC] [else : ExprC]) #:transparent)


(define slist (list `a `b `c `d `e `f `g `h))
(define (random-symbol [s : (Listof Symbol)]): Symbol
   (list-ref s (random (length s))))

; pick one from numc, idc, and stringc and then pick a number, identifier or string
(define (random-base-term) : ExprC
  (define x (random 3))
  (cond
    [(eq? x 0) (NumC (random))]
    [(eq? x 1) (IdC (random-symbol slist))]
    [else (StringC (list-ref '("a" "b" "c" "d" "e" "f") (random 6)))]))


; base case where the max_depth becomes 0 and then you call base term func to stop recursion
(define (random-term [max_depth : Integer]) : ExprC
  (cond
    [(eq? max_depth 0) (random-base-term)]
    )
  (define y (random 3))
  (cond
    [(eq? y 0) (LamC (build-list (random 3) slist) (random-term (- max_depth 1)))]
    [(eq? y 1) (AppC (random-term (- max_depth 1)) (build-list (random 3) (lambda (max_depth) (random-term (- max_depth 1)))))]
    [else (ifC (random-term (- max_depth 1)) (random-term (- max_depth 1)) (random-term (- max_depth 1)))]
  ))


(check-equal? (unparse (NumC 1)) 1)
(check-equal? (unparse (IdC `x)) `x)
(check-equal? (unparse (StringC "a")) "a")
(check-equal? (unparse (ifC (IdC `true) (NumC 1) (IdC `false))) '{if true 1 false})
(check-equal? (unparse (AppC (IdC `+) (list (NumC 1) (NumC 2)))) '{+ 1 2})
;(check-equal? (unparse ()))

#;(check-equal? (parse '{x => {+ x 2}})
              (LamC (list `x) (AppC (IdC `+) (list (IdC `x) (NumC 2)))))

#;(check-equal? (parse '{if true 1 false})
              (ifC (IdC `true) (NumC 1) (IdC `false)))

#;(check-equal? (parse '{+ 1 2})
              (AppC (IdC `+) (list (NumC 1) (NumC 2))))


(define (unparse [parsed_exp : ExprC]) : Sexp
  (match parsed_exp
    [(NumC n) n]
    [(IdC x) x]
    [(StringC str) str]
    [(LamC args body) (append args (cons `=> (list (unparse body))))]
    [(AppC fun args) (define arg (for/list : (Listof Sexp) ([i (cast args (Listof ExprC))]) (unparse (cast i ExprC)))) (cons (unparse fun) arg)]
    [(ifC if then else) (list 'if (unparse if) (unparse then) (unparse else))])
  )

(define (quiz)
  (unparse (random-term (random))))

(define secret (quiz))