#lang typed/racket
(require typed/rackunit)

;1. Parse000
(define (parse000 [s : Sexp]) : Boolean
  (match s
  [(list (? real? r) `chris (? symbol? s)) true]
  [other false]))

(check-true (parse000 (list 1 `chris `chris)))
(check-false (parse000 (list 1 `c `chris)))



;2. Parse001
(define-type Symbool(U Boolean Symbol))
(define (parse001 [s : Sexp]) : Symbool
  (match s
  [(list (? real? r) `chris (? symbol? s)) s]
  [other false]))

(check-equal? (parse001 (list 1 `chris `chris)) `chris)
(check-false (parse001 (list 1 `c `chris)))



;3. Parse002
(define (parse002 [s : Sexp]) : (U Boolean (Listof Real))
  (match s
  [(list a (list (? real? nums) ...) c) (cast nums (Listof Real))]
  [other #f]))

(check-equal? (parse002 (list 1 (list 1 2 3) `chris)) `(1 2 3))
(check-equal? (parse002 (list 1 2 `chris)) #f)



;4. OHNo
(define (ohno [value : Any]) : Symbol
  (cond
    [(real? value) `okay]
    [else (error 'bad-times "expected a real, got ~e" 1234)]))

(check-exn (regexp (regexp-quote "expected a real"))
           (lambda () (ohno "poop")))
(check-equal? (ohno 6) `okay)



;5. ArithC
(define-type ArithC (U NumC PlusC MultC))
(struct NumC ([n : Real]) #:transparent)
(struct PlusC ([l : ArithC] [r : ArithC]) #:transparent)
(struct MultC ([l : ArithC] [r : ArithC]) #:transparent)



;6. Interp
(define (interp [a : ArithC]) : Real
    (match a
      [(NumC n) n]
      [(PlusC l r) (+ (interp l) (interp r))]
      [(MultC  l r) (* (interp l) (interp r))]))

(check-equal? (interp (NumC 5)) 5)
(check-equal? (interp (PlusC (NumC 5) (NumC 5))) 10)
(check-equal? (interp (MultC (NumC 5) (NumC 5))) 25)
(check-equal? (interp (MultC (PlusC (NumC 5) (NumC 5)) (NumC 5))) 50)



;7. Num-adds
(define (num-adds [a : ArithC]) : Real
  (match a
    [(NumC n) 0]
    [(PlusC l r) (cond
                   [(and (NumC? l) (NumC? r)) 1]
                   [(NumC? l) (+ 1 (num-adds r))]
                   [(NumC? r) (+ 1 (num-adds l))]
                   [else (+ 1 (num-adds r) (num-adds l))])]
    [(MultC l r) (cond
                   [(and (NumC? l) (NumC? r)) 0]
                   [(NumC? l) (num-adds r)]
                   [(NumC? r) (num-adds l)]
                   [else (+ (num-adds r) (num-adds l))])]))

(check-equal? (num-adds (NumC 5)) 0)
(check-equal? (num-adds (PlusC (NumC 4) (NumC 5))) 1)



;8. parse1
(define-type ArithS (U NumS PlusS BMinusS MultS UMinusS))
(struct NumS ([n : Real]) #:transparent)
(struct PlusS ([l : ArithS] [r : ArithS]) #:transparent)
(struct BMinusS ([l : ArithS] [r : ArithS]) #:transparent)
(struct MultS ([l : ArithS] [r : ArithS]) #:transparent)
(struct UMinusS ([e : ArithS]) #:transparent)

(define (desugar [as : ArithS]) : ArithC
    (match as
      [(NumS n) (NumC n)]
      [(PlusS l r) (PlusC (desugar l) (desugar r))]
      [(MultS l r) (MultC (desugar l) (desugar r))]
      [(UMinusS e) (MultC (desugar e) (NumC -1))]
      [(BMinusS l r) (PlusC (desugar l) (MultC (NumC -1)
                                           (desugar r)))]))

(define (parse1 [s : Sexp]) : ArithS
  (match s
    [(? real? r) (NumS r)]
    [(list `+ r l) (PlusS (parse1 r) (parse1 l))]
    [(list `* r l) (MultS (parse1 r) (parse1 l))]
    [(list `- r l) (BMinusS (parse1 r) (parse1 l))]
    [(list `- r) (UMinusS (parse1 r))]
    [else (NumS 0)]))


(check-equal? (parse1 `(+ 1 2)) (PlusS (NumS 1) (NumS 2)))
(check-equal? (parse1 `(* 1 2)) (MultS (NumS 1) (NumS 2)))
(check-equal? (parse1 `(- 1 2)) (BMinusS (NumS 1) (NumS 2)))
(check-equal? (parse1 `(- 1)) (UMinusS (NumS 1)))



;9. Top-Interp
(define (top-interp [s : Sexp]) : Real
  (interp (desugar (parse1 s))))

(check-equal? (top-interp `(+ 1 2)) 3)



;10. Parse2
(define (parse2 [s : Sexp]) : ArithC
  (match s
    [(? real? r) (desugar (NumS r))]
    [(list `+ r l) (desugar (PlusS (parse1 r) (parse1 l)))]
    [(list `* r l) (desugar (MultS (parse1 r) (parse1 l)))]
    [(list `- r l) (PlusC (desugar l) (MultC (NumC -1)
                                           (desugar r)))]
    [(list `- r) (desugar (UMinusS (parse1 r)))]))

(check-equal? (parse2 `(+ 1 2)) (PlusC (NumC 1) (NumC 2)))