#lang typed/racket
(require typed/rackunit)
;Completed 0 Tests Fail!



;Structure Definitions
  ;ExprC
(define-type ExprC (U NumC IdC AppC BinopC leq0?C))
(struct NumC ([n : Real]) #:transparent)
(struct IdC ([s : Symbol]) #:transparent)
(struct AppC ([fun : Symbol] [args : (Listof ExprC)]) #:transparent)
  ;Binop
(struct BinopC ([op : Symbol] [l : ExprC] [r : ExprC]) #:transparent)
  ;FundefC
(struct FunDefC ([name : Symbol] [args : (Listof Symbol)] [body : ExprC]) #:transparent)
  ;leqo?C
(struct leq0?C ([if : ExprC] [then : ExprC] [else : ExprC]) #:transparent)



;Parsers================================================================================
  ; parse: s-expression -> ExprC
  ; It parses an s-expresson into an ExprC type if not then a JILI error is raised
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? r) (NumC r)]
    [(? symbol? s) (cond
                     [(equal? s `+) (error 'parse "JILI Unexpected symbol")]
                     [(equal? s `-) (error 'parse "JILI Unexpected symbol")]
                     [(equal? s `/) (error 'parse "JILI Unexpected symbol")]
                     [(equal? s `*) (error 'parse "JILI Unexpected symbol")]
                     [(equal? s `def) (error 'parse "JILI Unexpected symbol")]
                     [(equal? s `leq0?) (error 'parse "JILI Unexpected symbol")]
                     [else (IdC s)])]
    [(list l ...) (checkArgs l)]
    [other (error `parse "JILI Unexpected s-expression")]))



  ; checkArgs: Listof s-expression -> ExprC
  ; It parses a List of s-expressons into an ExprC type
(define (checkArgs [l : (Listof Sexp)]) : ExprC
  (cond
    [(empty? l) (error 'checkArgs "JILI No Arguments")]
    [else (define firstInList (first l))
          (cond
            [(symbol? firstInList)
             (cond
               [(equal? (member (first l) `(+ - / * leq0?)) #f)
                (cond
                  [(equal? 1 (length l)) (AppC firstInList `())]
                  [(and (symbol? (first l)) (equal? 2 (length l))) (AppC firstInList (list (parse (second l))))]
                  [else (AppC firstInList (getArgs (rest l)))])]
               [(equal? (first l) 'leq0?)
                (cond
                  [(equal? 4 (length l)) (leq0?C (parse (second l)) (parse (third l)) (parse (fourth l)))]
                  [else (error 'checkArgs "JILI Wrong number of arguments for leq0?")])]
               [else
                (cond
                  [(equal? 3 (length l)) (BinopC firstInList (parse (second l)) (parse (third l)))]
                  [else (error 'checkArgs "JILI Wrong number of arguments for Binop")])])]
            [else (error 'checkArgs "JILI Wrong format for function")])]))



  ; parse-fundef: s-expression -> FunDefC
  ; A function that parses function definitions as s-expressions into FunDefC's
(define (parse-fundef [s : Sexp]) : FunDefC
  (match s
    [(list `def (list (? symbol? l) ...) body)
     (FunDefC (cast (first l) Symbol) (cast (rest l) (Listof Symbol)) (parse body))]
    [else (error 'parse-fundef "JILI Empty FuncDefC")]))



  ; parse-prog: s-expression -> FunDefC
  ; A function that parses programs as s-expressions into FunDefC's
(define (parse-prog [s : Sexp]) : (Listof FunDefC)
  (match s
    [`() `()]
    [(cons f r) (cons (parse-fundef f) (parse-prog r))]))



;Interps================================================================================
  ; interp: ExprC, (Listof FunDefC) -> Real
  ; A function that referenceds a Listof FunDefC's and interprets an ExprC into a real
(define (interp [exp : ExprC][funs : (Listof FunDefC)]) : Real
  (match exp
    [(NumC n) n]
    [(BinopC op l r) (getBinop op (interp l funs) (interp r funs))]
    [(leq0?C if then else) (cond
                             [(>= 0 (interp if funs)) (interp then funs)]
                             [else (interp else funs)])]
    [(AppC fun args) (cond
                       [(equal? 1 (length args))
                        (cond
                          [(equal? 1 (length
                                      (FunDefC-args (get-fundef fun funs))))
                           (local ([define fd (get-fundef fun funs)])
                             (interp (subst (NumC (interp (first args) funs))
                                            (first (FunDefC-args fd))
                                            (FunDefC-body fd))
                                     funs))]
                          [else (error 'interp "JILI Too many args")])]
                       [(equal? 0 (length args)) (local ([define fd (get-fundef fun funs)])
                                                   (interp (FunDefC-body fd) funs))]
                       [else (local ([define fd (get-fundef fun funs)])
                               (interp (substArgs args
                                                  (FunDefC-args fd)
                                                  (FunDefC-body fd))
                                       funs))])]
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
            [(equal? `main (FunDefC-name (first funs)))
             (cond
               [(empty? (FunDefC-args (first funs))) (interp (FunDefC-body (first funs)) funs-og)]
               [else (error 'helper-interp-fns "JILI Main should have no args")])]
            [else (helper-interp-fns (rest funs) funs-og)])]))



  ; top-interp: s-expression -> Real 
  ; Function that interprets at the top level and reads in the raw s-expressions to evaluate to Reals
(define (top-interp [fun-sexps : Sexp]) : Real
  (interp-fns (parse-prog fun-sexps)))



;Helper Functions================================================================================
  ; getArgs: (Listof s-expression) -> (Listof ExprC)
  ; It parses a listof s-expressons into a listof ExprC type if not then a JILI error is raised
(define (getArgs [s : (Listof Sexp)]) : (Listof ExprC)
  (match s
    [`() `()]
    [(cons f r) (cons (parse f) (getArgs r))]))



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
    [(AppC f a) (AppC f (substInMany what for a))]
    [(BinopC op l r) (BinopC op (subst what for l) (subst what for r))]
    [(leq0?C if then else) (leq0?C (subst what for if) (subst what for then) (subst what for else))]))



  ; substArgs: (Listof ExprC), (Listof Symbol), ExprC -> ExprC
  ; A function that replaces an ExprC with a Symbol in a list of ExprC's
(define (substInMany [what : ExprC] [for : Symbol] [args : (Listof ExprC)]) : (Listof ExprC)
  (match args
    [`() args]
    [(cons f r) (cons (subst what for f) (substInMany what for r))]))
       


  ; substArgs: (Listof ExprC), (Listof Symbol), ExprC -> ExprC
  ; A function that replaces a list of Symbols for a list of ExprC's within an ExprC
(define (substArgs [args : (Listof ExprC)] [fors : (Listof Symbol)] [in : ExprC]) : ExprC
  (match args
    [`() in]
    [else (cond
            [(equal? (length args) (length fors))
             (substArgs (rest args) (rest fors) (subst (first args) (first fors) in))]
            [else (error 'substArgs "JILI Too many args")])]))



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



;Testing Suite================================================================================
  ;Testing Parse
(check-equal? (parse `x) (IdC `x))
(check-equal? (parse 6) (NumC 6))
(check-equal? (parse `(f x)) (AppC `f (list (IdC `x))))
(check-equal? (parse `(a b (list c `()))) (AppC `a (list (IdC `b) (AppC `c (list)))))
(check-equal? (parse `(f)) (AppC `f `()))
(check-equal? (parse `(+ 1 2)) (BinopC `+ (NumC 1) (NumC 2)))
(check-equal? (parse `(f 2)) (AppC `f (list (NumC 2))))
(check-equal? (parse `(f x y)) (AppC `f (list (IdC `x) (IdC `y))))
(check-equal? (parse `{leq0? x 0 1}) (leq0?C (IdC `x) (NumC 0) (NumC 1)))
(check-equal? (parse `{leq0? {+ x 1} 0 1}) (leq0?C (BinopC `+ (IdC `x) (NumC 1)) (NumC 0) (NumC 1)))
(check-exn (regexp (regexp-quote "JILI Unexpected s-expression"))
           (lambda () (parse "a")))
(check-exn (regexp (regexp-quote "JILI Unexpected symbol"))
           (lambda () (parse `(+ + 3))))
(check-exn (regexp (regexp-quote "JILI Unexpected symbol"))
           (lambda () (parse `(+ - 3))))
(check-exn (regexp (regexp-quote "JILI Unexpected symbol"))
           (lambda () (parse `(+ * 3))))
(check-exn (regexp (regexp-quote "JILI Unexpected symbol"))
           (lambda () (parse `(+ / 3))))
(check-exn (regexp (regexp-quote "JILI Unexpected symbol"))
           (lambda () (parse `(+ leq0? 3))))
(check-exn (regexp (regexp-quote "JILI Wrong number of arguments for leq0?"))
           (lambda () (parse `(leq0? 3 4 5 6))))
(check-exn (regexp (regexp-quote "JILI Wrong number of arguments for Binop"))
           (lambda () (parse `(/ 3 4 5))))
(check-exn (regexp (regexp-quote "JILI Unexpected symbol"))
           (lambda () (parse `(+ def a))))



  ;Testing Parse-FundefC
(check-equal? (parse-fundef `{def {main} {+ 1 2}}) (FunDefC `main `() (BinopC `+ (NumC 1) (NumC 2))))
(check-equal? (parse-fundef `{def {f x} {+ x 2}}) (FunDefC `f (list `x) (BinopC `+ (IdC `x) (NumC 2))))
(check-equal? (parse-fundef `{def {f} {+ 1 2}}) (FunDefC `f `() (BinopC `+ (NumC 1) (NumC 2))))
(check-equal? (parse-fundef `{def {main} {f 2}}) (FunDefC `main `() (AppC `f (list (NumC 2)))))
(check-exn (regexp (regexp-quote "JILI Empty FuncDefC"))
           (lambda () (parse-fundef `())))
(define test_list_funDefC01(list
                            (FunDefC 'f '(x) (BinopC '+ (IdC 'x) (NumC 14)))
                            (FunDefC 'main '() (AppC 'f (list (NumC 2))))))



  ;Testing Parse-Prog
(check-equal? (parse-prog `((def (main) (f 9))
                            (def (f x) (g 3 x))
                            (def (g a b) (+ a b))))
              (list (FunDefC `main `() (AppC `f (list (NumC 9))))
                    (FunDefC `f (list `x) (AppC `g (list (NumC 3) (IdC `x))))
                    (FunDefC `g (list `a `b) (BinopC `+ (IdC `a) (IdC `b)))))
(check-equal? (parse-prog `{{def {f x} {+ x 2}}
                            {def {main} {f 2}}})
              (list (FunDefC `f (list `x) (BinopC `+ (IdC `x) (NumC 2)))
                    (FunDefC `main `() (AppC `f (list (NumC 2))))))
(check-equal? (parse-prog `{{def {f} {+ 1 2}}
                            {def {main} {f}}})
              (list (FunDefC `f `() (BinopC `+ (NumC 1) (NumC 2)))
                    (FunDefC `main `() (AppC `f `()))))
(check-exn (regexp (regexp-quote "JILI Empty FuncDefC"))
           (lambda () (parse-prog `("trash"))))
(check-equal? (parse-prog '{{def {f x} {+ x 14}}
                            {def {main} {f 2}}})
              test_list_funDefC01)
(check-equal? (parse-prog (quote (
                                  (def (even? x) (leq0? x 1 (odd? (- x 1))))
                                  (def (odd? x) (leq0? x 0 (even? (- x 1))))
                                  (def (leq? x y) (leq0? (- x y) 1 0))
                                  (def (main) (even? 378)))))
              (list
               (FunDefC `even? (list `x) (leq0?C (IdC `x) (NumC 1) (AppC `odd? (list (BinopC `- (IdC `x) (NumC 1))))))
               (FunDefC `odd? (list `x) (leq0?C (IdC `x) (NumC 0) (AppC `even? (list (BinopC `- (IdC `x) (NumC 1))))))
               (FunDefC `leq? (list `x `y) (leq0?C (BinopC `- (IdC `x) (IdC `y)) (NumC 1) (NumC 0)))
               (FunDefC `main `() (AppC `even? (list (NumC 378))))))



  ;Testing Interp
(check-equal? (interp (BinopC `+ (NumC 1) (NumC 2)) `()) 3)
(check-equal? (interp (AppC `f (list (NumC 1) (NumC 2))) (list
                                                          (FunDefC `f (list `x `y) (BinopC `+ (IdC `x) (IdC `y))))) 3)
(check-equal? (interp (AppC `g (list (NumC 1) (NumC 1))) (list
                                                          (FunDefC `g (list `a `b) (BinopC `+ (IdC `a) (IdC `b))))) 2)
(check-equal? (interp (AppC `f (list (NumC 9))) (list
                                                 (FunDefC `f (list `x) (AppC `g (list (NumC 3) (IdC `x))))
                                                 (FunDefC `g (list `a `b) (BinopC `+ (IdC `a) (IdC `b))))) 12)
(check-equal? (interp (AppC `main `()) (list
                                        (FunDefC `main `() (AppC `f (list (NumC 9))))
                                        (FunDefC `f (list `x) (AppC `g (list (NumC 3) (IdC `x))))
                                        (FunDefC `g (list `a `b) (BinopC `+ (IdC `a) (IdC `b))))) 12)
(check-equal? (interp (AppC `even? (list (NumC 378))) (parse-prog (quote
                                                                   ((def (even? x) (leq0? x 1 (odd? (- x 1))))
                                                                    (def (odd? x) (leq0? x 0 (even? (- x 1))))
                                                                    (def (leq? x y) (leq0? (- x y) 1 0))
                                                                    (def (main) (even? 378))))))1)
(check-equal? (interp (AppC `even? (list (NumC 378)))
                      (list
                       (FunDefC `even? (list `x) (leq0?C (IdC `x) (NumC 1)
                                                         (AppC `odd? (list (BinopC `- (IdC `x) (NumC 1))))))
                       (FunDefC `odd? (list `x) (leq0?C (IdC `x) (NumC 0)
                                                        (AppC `even? (list (BinopC `- (IdC `x) (NumC 1))))))
                       (FunDefC `leq? (list `x `y) (leq0?C
                                                    (BinopC `- (IdC `x) (IdC `y)) (NumC 1) (NumC 0)))
                       (FunDefC `main `() (AppC `even? (list (NumC 378))))))1)
(check-equal? (interp (AppC `f `()) (list (FunDefC `f `() (BinopC `+ (NumC 1) (NumC 2))))) 3)
(check-exn (regexp (regexp-quote "JILI Division by zero in getBinop"))
           (lambda () (interp (BinopC `/ (NumC 1) (BinopC `+ (NumC 0) (NumC 0))) `())))
(check-exn (regexp (regexp-quote "JILI Division by zero in getBinop"))
           (lambda () (interp (AppC `f (list (BinopC `/ (NumC 1) (BinopC `+ (NumC 0)(NumC 0)))))
                              (list (FunDefC `f `(x) (BinopC `+ (NumC 1) (NumC 1)))))))
(check-exn (regexp (regexp-quote "JILI shouldn't get here in interp"))
           (lambda () (interp (IdC `s) `())))

(define test_list_funDefC02(list
                            (FunDefC 'f (list 'x) (BinopC '+ (IdC 'x) (NumC 14)))))
(check-equal?(interp (NumC 2) test_list_funDefC02 )2)



  ;Testing Interp-FNS
(check-equal? (interp-fns (list (FunDefC `main  `() (BinopC `+ (NumC 1) (NumC 2))))) 3)
(check-equal? (interp-fns
               (parse-prog '{{def {main} {+ 1 2}}}))
              3)
(check-equal? (interp-fns
               (parse-prog '{{def {f x y} {+ x y}}
                             {def {main} {f 1 2}}}))
              3)
(check-equal? (interp-fns
               (parse-prog '{{def {f} 5}
                             {def {main} {+ {f} {f}}}}))
              10)
(check-equal? (interp-fns test_list_funDefC01) 16)
(check-equal? (interp-fns test_list_funDefC02) 0)



  ;Testing Top-Interp
(check-equal? (top-interp '{}) 0)
(check-equal? (top-interp '{{def {main} {+ 1 2}}}) 3)
(check-equal? (top-interp '{{def {main} {* 1 2}}}) 2)
(check-equal? (top-interp '{{def {main} {- 1 2}}}) -1)
(check-equal? (top-interp '{{def {main} {/ 1 2}}}) 1/2)
(check-equal? (top-interp '{{def {f x} {+ x 14}}
                            {def {main} {f 2}}}) 16)
(check-equal? (top-interp (quote ((def (main) (f 9)) (def (f x) (g 3 x)) (def (g a b) (+ a b))))) 12)
(check-equal? (top-interp '{{def {minus-five x} {+ x (* -1 5)}} {def {main} {minus-five {+ 8 0}}} }) 3)
(check-equal? (top-interp (quote ((def (main) (leq0? (* 3 1) 3 (+ 2 9)))))) 11)
(check-equal? (top-interp (quote ((def (main) (leq0? (- 1 2) 3 (+ 2 9)))))) 3)
(check-exn (regexp (regexp-quote "JILI Division by zero in getBinop"))
           (lambda () (top-interp '((def (ignoreit x) (+ 3 4)) (def (main) (ignoreit (/ 1 (+ 0 0))))))))
(check-exn (regexp (regexp-quote "JILI Main should have no args"))
           (lambda () (top-interp '((def (ignoreit x) (+ 3 4)) (def (main init) (ignoreit (/ 1 (+ 0 0))))))))
(check-exn (regexp (regexp-quote "JILI Too many args"))
           (lambda () (top-interp '((def (f x y) (+ x 2)) (def (main) (f 3))))))
(check-exn (regexp (regexp-quote "JILI Too many args"))
           (lambda () (top-interp '((def (f x y) (+ x 2)) (def (main) (f 3 4 5))))))
(check-equal? (top-interp (quote (
                                  (def (even? x) (leq0? x 1 (odd? (- x 1))))
                                  (def (odd? x) (leq0? x 0 (even? (- x 1))))
                                  (def (leq? x y) (leq0? (- x y) 1 0))
                                  (def (main) (even? 378))))) 1)



;Testing Helper Funcs================================================================================
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
(check-equal? (subst (NumC 1) `x (IdC `x)) (NumC 1))
(check-equal? (subst (NumC 1) `f (IdC `x)) (IdC `x))
(check-equal? (subst (NumC 1) `x (leq0?C (IdC `x) (IdC `x) (NumC 0))) (leq0?C (NumC 1) (NumC 1) (NumC 0)))
(check-equal? (subst (NumC 1) `x (AppC `f (list (IdC `x)))) (AppC `f (list (NumC 1))))
(check-equal? (subst (NumC 1) `x (AppC `f (list (IdC `x) (NumC 2)))) (AppC `f (list (NumC 1) (NumC 2))))
(check-equal? (subst (NumC 378) `x (leq0?C (IdC `x) (NumC 1) (AppC `odd (list (BinopC `- (IdC `x) (NumC 1))))))
              (leq0?C (NumC 378) (NumC 1) (AppC `odd (list (BinopC `- (NumC 378) (NumC 1))))))
(check-equal? (subst (NumC 378) `x (AppC `odd (list (BinopC `- (IdC `x) (NumC 1)))))
              (AppC `odd (list (BinopC `- (NumC 378) (NumC 1)))))



  ;Testing SubstInMany
(check-equal? (substInMany (NumC 1) `x (list (IdC `x))) (list (NumC 1)))
(check-equal? (substInMany (NumC 378) `x (list (BinopC `- (IdC `x) (NumC 1))))
              (list (BinopC `- (NumC 378) (NumC 1))))



  ;Testing SubstArgs
(check-equal? (substArgs (list (NumC 1)) (list `x)
                         (AppC `f (list (IdC `x)))) (AppC `f (list (NumC 1))))
(check-equal? (substArgs (list (NumC 1) (NumC 2)) (list `x `y)
                         (BinopC `+ (IdC `x) (IdC `y))) (BinopC `+ (NumC 1) (NumC 2)))
(check-equal? (substArgs (list (NumC 1) (NumC 2)) (list `x `y)
                         (AppC `f (list (IdC `x) (IdC `y)))) (AppC `f (list (NumC 1) (NumC 2))))
(check-equal? (substArgs (list (NumC 1)) (list `x)
                         (AppC `f (list (IdC `x) (NumC 2)))) (AppC `f (list (NumC 1) (NumC 2))))



  ;Testing checkArgs
(check-exn (regexp (regexp-quote "JILI No Arguments"))
           (lambda () (checkArgs `())))
(check-exn (regexp (regexp-quote "JILI Wrong format for function"))
           (lambda () (checkArgs `("a"))))



  ;Testing GetFundef
(check-equal? (get-fundef `main (list (FunDefC `f `(x) (BinopC `+ (NumC 1) (NumC 2)))
                                      (FunDefC `main `() (BinopC `+ (NumC 1) (NumC 2)))))
              (FunDefC `main `() (BinopC `+ (NumC 1) (NumC 2))))
(check-exn (regexp (regexp-quote "JILI reference to undefined function"))
           (lambda () (get-fundef `s `())))