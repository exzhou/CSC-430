#lang typed/racket
(require typed/rackunit)
;Completed 0 Tests Fail!


;Structure Definitions
  ;ExprC
(define-type ExprC (U NumC IdC AppC BinopC leq0?C))
(struct NumC ([n : Real]) #:transparent)
(struct IdC ([s : Symbol]) #:transparent)
(struct AppC ([fun : Symbol] [arg : ExprC]) #:transparent)
  ;leq0?C
(struct leq0?C ([if : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
  ;Binop
(struct BinopC ([op : Symbol] [l : ExprC] [r : ExprC]) #:transparent)
  ;FundefC
(struct FunDefC ([name : Symbol] [arg : Symbol] [body : ExprC]) #:transparent)



;Parsers
  ; parse: s-expression -> ExprC
  ; It parses an s-expresson into an ExprC type if not then a JILI error is raised
(define (parse [s : Sexp]) : ExprC
  (match s
    [(list `leq0? if then else) (leq0?C (parse if) (parse then) (parse else))]
    [(? real? r) (NumC r)]
    [(list (? symbol? fun) arg) (AppC fun (parse arg))]
    [(list (? symbol? op) r l) (BinopC op (parse r) (parse l))]
    [(? symbol? s) (cond
                     [(equal? s `+) (error `JILI "JILI Unexpected symbol in Parse")]
                     [(equal? s `-) (error `JILI "JILI Unexpected symbol in Parse")]
                     [(equal? s `/) (error `JILI "JILI Unexpected symbol in Parse")]
                     [(equal? s `*) (error `JILI "JILI Unexpected symbol in Parse")]
                     [(equal? s `def) (error `JILI "JILI Unexpected symbol in Parse")]
                     [(equal? s `leq0?) (error `JILI "JILI Unexpected symbol in Parse")]
                     [else (IdC s)])]
    [else (error `JILI "JILI Unexpected s-expression in Parse")]))
    


  ; parse-fundef: s-expression -> FunDefC
  ; A function that parses function definitions as s-expressions into FunDefC's
(define (parse-fundef [s : Sexp]) : FunDefC
  (match s
    [(list `def (list (? symbol? name) (? symbol? arg)) body) (FunDefC name arg (parse body))]
    [else (error 'JILI "JILI Empty FuncDefC in parse-fundef")]));Add empty case



  ; parse-prog: s-expression -> FunDefC
  ; A function that parses programs as s-expressions into FunDefC's
(define (parse-prog [s : Sexp]) : (Listof FunDefC)
  (match s
    [`() `()]
    [(cons f r) (cons (parse-fundef f) (parse-prog r))]))



;Interps
  ; interp: ExprC, (Listof FunDefC) -> Real
  ; A function that referenceds a Listof FunDefC's and interprets an ExprC into a real
(define (interp [exp : ExprC][funs : (Listof FunDefC)]) : Real
  (match exp
    [(NumC n) n]
    [(BinopC op l r) (getBinop op (interp l funs) (interp r funs))]
    [(leq0?C if then else) (cond
                             [(>= 0 (interp if funs)) (interp then funs)]
                             [else (interp else funs)])]
    [(AppC fun arg) (local ([define fd (get-fundef fun funs)])
                      (interp (subst (NumC (interp arg funs))
                                     (FunDefC-arg fd)
                                     (FunDefC-body fd))
                              funs))]
    [(IdC s) (error `JILI "JILI shouldn't get here in interp")]))



  ; interp-fns: (Listof FunDefC) -> Real
  ; A function that interprets a Listof FunDefC's and calls on helper to find `main and evaluate it to a real
(define (interp-fns [funs : (Listof FunDefC)]) : Real
  (helper-interp-fns funs funs))



  ; helper-interp-fns: (Listof FunDefC), (Listof FunDefC) -> Real 
  ; A helper function that takes two Listof FunDefC's and evaluates the `main function as a Real
(define (helper-interp-fns [funs : (Listof FunDefC)] [funs-og : (Listof FunDefC)]) : Real
  (match funs
    [`() 0]
    [else (cond
            [(and (equal? `init (FunDefC-arg (first funs)))
                  (equal? `main (FunDefC-name (first funs))))
             (interp (subst (NumC 0) `init (FunDefC-body (first funs))) funs-og)]
            [else (helper-interp-fns (rest funs) funs-og)])]))



  ; top-interp: s-expression -> Real
  ; Function that interprets at the top level and reads in the raw s-expressions to evaluate to Reals
(define (top-interp [fun-sexps : Sexp]) : Real
  (interp-fns (parse-prog fun-sexps)))



;Helper Functions
  ; getBinop: Symbol, Real, Real -> Real
  ; Function that evaluates the binary operation being applied and evaluates the opaeration to a Real
(define (getBinop [binop : Symbol] [l : Real] [r : Real]) : Real
  (cond
    [(equal? `+ binop) (+ l r)]
    [(equal? `* binop) (* l r)]
    [(equal? `- binop) (- l r)]
    [(equal? `/ binop) (cond
                         [(equal? r 0) (error `JILI "JILI Division by zero in getBinop")]
                         [else (/ l r)])]
    [else (error `JILI "JILI Unexpected symbol in getBinop")]))



  ; subst: ExprC, Symbol, ExprC -> ExprC
  ; A function that takes a what to replace for and in where and evaluates to the resulant ExprC
(define (subst [what : ExprC] [for : Symbol] [in : ExprC]) : ExprC
  (match in
    [(NumC n) in]
    [(IdC s) (cond
               [(symbol=? s for) what]
               [else in])]
    [(AppC f a) (AppC f (subst what for a))]
    [(BinopC op l r) (BinopC op (subst what for l) (subst what for r))]
    [(leq0?C if then else) (leq0?C (subst what for if) (subst what for then) (subst what for else))]))



  ; get-fundef: Symbol, (Listof FunDefC)  -> FunDefC
  ; Function that finds a target FunDefC given a list of function definitions and a target symbol
(define (get-fundef [n : Symbol] [fds : (Listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds)
     (error `JILI "JILI reference to undefined function")]
    [(cons? fds)
     (cond
       [(equal? n (FunDefC-name (first fds))) (first fds)]
       [else (get-fundef n (rest fds))])]))



;Testing Suite
  ;Testing Parse
(check-equal? (parse `x) (IdC `x))
(check-equal? (parse 6) (NumC 6))
(check-equal? (parse `(f x)) (AppC `f (IdC `x)))
(check-equal? (parse `(+ 1 2)) (BinopC `+ (NumC 1) (NumC 2)))
(check-exn (regexp (regexp-quote "JILI Unexpected s-expression in Parse"))
           (lambda () (parse `(/ 3 4 5))))
(check-exn (regexp (regexp-quote "JILI Unexpected symbol in Parse"))
           (lambda () (parse `(+ + 3))))
(check-exn (regexp (regexp-quote "JILI Unexpected symbol in Parse"))
           (lambda () (parse `(+ - 3))))
(check-exn (regexp (regexp-quote "JILI Unexpected symbol in Parse"))
           (lambda () (parse `(+ * 3))))
(check-exn (regexp (regexp-quote "JILI Unexpected symbol in Parse"))
           (lambda () (parse `(+ / 3))))
(check-exn (regexp (regexp-quote "JILI Unexpected symbol in Parse"))
           (lambda () (parse `(+ leq0? 3))))
(check-exn (regexp (regexp-quote "JILI Unexpected symbol in Parse"))
           (lambda () (parse `(+ def a))))
(check-equal? (parse `{leq0? x 0 1}) (leq0?C (IdC `x) (NumC 0) (NumC 1)))



  ;Testing Parse-FundefC
(check-exn (regexp (regexp-quote "JILI Empty FuncDefC in parse-fundef"))
           (lambda () (parse-fundef `())))
(define test_list_funDefC01(list
                            (FunDefC 'f 'x (BinopC '+ (IdC 'x) (NumC 14)))
                            (FunDefC 'main 'init (AppC 'f (NumC 2)))))



  ;Testing Parse-Prog
(check-exn (regexp (regexp-quote "JILI Empty FuncDefC in parse-fundef"))
           (lambda () (parse-prog `("trash"))))
(check-equal? (parse-prog '{{def {f x} {+ x 14}}
                            {def {main init} {f 2}}})
              test_list_funDefC01)



  ;Testing Interp
(check-exn (regexp (regexp-quote "JILI Division by zero in getBinop"))
           (lambda () (interp (BinopC `/ (NumC 1) (BinopC `+ (NumC 0) (NumC 0))) `())))
(check-exn (regexp (regexp-quote "JILI Division by zero in getBinop"))
           (lambda () (interp (AppC `f (BinopC `/ (NumC 1) (BinopC `+ (NumC 0)(NumC 0))))
                              (list (FunDefC `f `x (BinopC `+ (NumC 1) (NumC 1)))))))
(check-exn (regexp (regexp-quote "JILI shouldn't get here in interp"))
           (lambda () (interp (IdC `s) `())))

(define test_list_funDefC02(list
                            (FunDefC 'f 'x (BinopC '+ (IdC 'x) (NumC 14)))))
(check-equal?(interp (NumC 2) test_list_funDefC02 )2)



  ;Testing Interp-FNS
(check-equal? (interp-fns test_list_funDefC01) 16)
(check-equal? (interp-fns test_list_funDefC02) 0)



  ;Testing Top-Interp
(check-equal? (top-interp '{}) 0)
(check-equal? (top-interp '{{def {main init} {+ 1 2}}}) 3)
(check-equal? (top-interp '{{def {main init} {* 1 2}}}) 2)
(check-equal? (top-interp '{{def {main init} {- 1 2}}}) -1)
(check-equal? (top-interp '{{def {main init} {/ 1 2}}}) 1/2)
(check-equal? (top-interp '{{def {f x} {+ x 14}}
                            {def {main init} {f 2}}}) 16)
(check-equal? (top-interp '{{def {minus-five x} {+ x (* -1 5)}} {def {main init} {minus-five {+ 8 init}}} }) 3)
(check-equal? (top-interp (quote ((def (main init) (leq0? (* 3 1) 3 (+ 2 9)))))) 11)
(check-equal? (top-interp (quote ((def (main init) (leq0? (- 1 2) 3 (+ 2 9)))))) 3)
(check-exn (regexp (regexp-quote "JILI Division by zero in getBinop"))
           (lambda () (top-interp '((def (ignoreit x) (+ 3 4)) (def (main init) (ignoreit (/ 1 (+ 0 0))))))))



;Testing Helper Funcs
  ;Testing helper-interp-fns
(check-equal? (helper-interp-fns test_list_funDefC01  test_list_funDefC01) 16)
(check-equal? (helper-interp-fns test_list_funDefC02  test_list_funDefC02) 0)



  ;Testing GetBinop
(check-equal? (getBinop `+ 1 2) 3)
(check-equal? (getBinop `* 1 2) 2)
(check-equal? (getBinop `- 1 2) -1)
(check-equal? (getBinop `/ 1 2) 1/2)
(check-exn (regexp (regexp-quote "JILI Unexpected symbol in getBinop"))
           (lambda () (getBinop `! 1 2)))
(check-exn (regexp (regexp-quote "JILI Division by zero in getBinop"))
           (lambda () (getBinop `/ 1 0)))



  ;Testing Subst
(check-equal? (subst (NumC 1) `x (BinopC `+ (IdC `x) (NumC 2))) (BinopC `+ (NumC 1) (NumC 2)))
(check-equal? (subst (NumC 1) `x (AppC `f (IdC `x))) (AppC `f (NumC 1)))
(check-equal? (subst (NumC 1) `x (IdC `x)) (NumC 1))
(check-equal? (subst (NumC 1) `f (IdC `x)) (IdC `x))
(check-equal? (subst (NumC 1) `f (leq0?C (IdC `f) (NumC 0) (NumC 1))) (leq0?C (NumC 1) (NumC 0) (NumC 1)))



  ;Testing GetFundef
(check-equal? (get-fundef `main (list (FunDefC `f `x (BinopC `+ (NumC 1) (NumC 2)))
                                      (FunDefC `main `init (BinopC `+ (NumC 1) (NumC 2)))))
              (FunDefC `main `init (BinopC `+ (NumC 1) (NumC 2))))
(check-exn (regexp (regexp-quote "JILI reference to undefined function"))
           (lambda () (get-fundef `s `())))