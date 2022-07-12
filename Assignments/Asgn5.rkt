#lang typed/racket
(require typed/rackunit)
;Completed 0 tests fail!

;Structure Definitions
  ;ExprC
(define-type ExprC (U NumC IdC StringC LamC AppC ifC))
(struct NumC ([n : Real]) #:transparent)
(struct IdC ([s : Symbol]) #:transparent)
(struct StringC ([s : String]) #:transparent)
(struct LamC ([args : (Listof Symbol)] [body : ExprC]) #:transparent)
(struct AppC ([fun : ExprC] [args : (Listof ExprC)]) #:transparent)
(struct ifC ([if : ExprC] [then : ExprC] [else : ExprC]) #:transparent)

  ;Binding
(struct Binding ([key : Symbol] [value : Value]) #:transparent)
(define-type Env (Listof Binding))

  ;Value
(define-type Value (U NumV BoolV CloV StringV OpV))
(struct NumV ([n : Real]) #:transparent)
(struct BoolV ([n : Boolean]) #:transparent)
(struct StringV ([s : String]) #:transparent)
(struct CloV ([args : (Listof Symbol)] [body : ExprC] [env : Env]) #:transparent)
(struct OpV ([op : Symbol]) #:transparent)
(define topEnv (list (Binding `+ (OpV `+))
                     (Binding `- (OpV `-))
                     (Binding `/ (OpV `/))
                     (Binding `* (OpV `*))
                     (Binding `true (BoolV true))
                     (Binding `false (BoolV false))
                     (Binding `<= (OpV `<=))
                     (Binding `equal? (OpV `equal?))
                     (Binding `error (OpV `error))))
(define-type Operator (U '+ '- '* '/ 'true 'false '<= 'equal? 'error))



;Parsers================================================================================
  ; parse: s-expression -> ExprC
  ; It parses an s-expresson into an ExprC type if not then a JILI error is raised
(define (parse [s : Sexp]) : ExprC
  (match s
    [`() (error 'parse "JILI Empty S-expression")]
    [(? real? r) (NumC r)]
    [(? string? str) (StringC str)]
    [(list `if if then else) (ifC (parse if) (parse then) (parse else))]
    [(list l ...) (checkArgs l)]
    [(? symbol? s) (cond
                     [(member s `(if var in =>)) (error 'parse "JILI Unexpected symbol")]
                     [else (IdC s)])]))



;Interps================================================================================
  ; interp: ExprC, (Listof FunDefC) -> Real
  ; A function that referenceds a Listof FunDefC's and interprets an ExprC into a real
(define (interp [exp : ExprC] [env : Env]) : Value
  (match exp
    [(NumC n) (NumV n)]
    [(IdC s) (lookup s env)]
    [(StringC str) (StringV str)]
    [(LamC args body) (CloV args body env)]
    [(ifC if then else) (cond
                          [(BoolV? (interp if env))
                           (match (BoolV-n (cast (interp if env) BoolV))
                             [#t (interp then env)]
                             [#f (interp else env)])]
                          [else (error 'inter "JILI Improper ifC")])]
    [(AppC fun arguments)
     (define fd (interp fun env))
     (match fd
       [(OpV op) (cond
                   [(equal? 2 (length arguments))
                    (getBinop op (interp (first arguments) env) (interp (second arguments) env))]
                   [(and (equal? 1 (length arguments)) (equal? `error op))
                    (error 'interp "JILI user-error ~v" (first arguments))] 
                  [else (error 'interp "JILI bad OpV")])]
       [(CloV args body clovEnv)
        (interp body (extend-env (map (lambda ([x : ExprC]) (interp x env)) arguments) args clovEnv))]
       [other (error 'interp "JILI Improper AppC")])]))



  ; top-interp: s-expression -> Real 
  ; Function that interprets at the top level and reads in the raw s-expressions to evaluate to Reals
(define (top-interp [fun-sexps : Sexp]) : String
  (serialize (interp (parse fun-sexps) topEnv)))



;Envrionment and new Asgn 5 Stuff================================================================================
  ; lookup: Symbol, Env -> Value
  ; A function that looksup the symbol in the Env and gets the binded Value
(define (lookup [for : Symbol] [env : Env]) : Value
  (cond
    [(empty? env) (error 'lookup "JILI name not found: ~e" for)]
    [else (cond
            [(equal? for (Binding-key (first env)))
             (Binding-value (first env))]
            [else (lookup for (rest env))])]))



  ; extend-env: (Listof ExprC), (Listof Symbol), Env-> Env
  ; A function that takes an exising Env and adds new bindings created from the vals and args
(define (extend-env [vals : (Listof Value)] [args : (Listof Symbol)] [curEnv : Env]) : Env
  (match vals
    [`() curEnv]
    [else (cond
            [(equal? (length vals) (length args)) (cons (Binding (first args) (first vals))
                                                           (extend-env (rest vals) (rest args) curEnv))]
            [else (error 'extend-env "JILI Too many args")])]))



  ; serialize: Value -> String
  ; A function taht takes a Value and serializes it into a String
(define (serialize [val : Value]) : String
  (match val
    [(NumV val) (~v val)]
    [(StringV val) (string-append-immutable "\"" val "\"")]
    [(BoolV val) (cond
                   [val "true"]
                   [else "false"])]
    [(CloV body ids env) "#<procedure>"]
    [(OpV op) "#<primop>"]))



;Helper Functions================================================================================
  ; checkArgs: Listof s-expression -> ExprC
  ; It parses a List of s-expressons into an ExprC type
(define (checkArgs [l : (Listof Sexp)]) : ExprC
  (match l                                  
    [`() (error 'checkArgs "JILI No Arguments")]
    [(list symbs ... `=> body)
     (match symbs
       [(list (? symbol? items) ...)
        (if
         (checkDuplicates (cast symbs (Listof Symbol)))
         (LamC (cast symbs (Listof Symbol)) (parse body))
         (error 'checkArgs "JILI Duplicate arg symbols"))]
       [else (error 'checkArgs "JILI Arguments Have Improper Format")])]
    [(list (? symbol? symb) `= (list body ...)) (LamC (list symb) (parse body))]
    [(list `var (list vars ...) `in body) (define symbols (getVars vars))
                                          (if (checkDuplicates symbols)
                                              (AppC (LamC symbols (parse body)) (getArgs vars))
                                              (error 'checkArgs "JILI Duplicates"))]
    [(list l ...)
     (AppC (parse (first l))
           (cast (map (lambda ([x : Sexp]) (parse x)) (cast (rest l) (Listof Sexp))) (Listof ExprC)))]))



  ; getBinop: Symbol, Value, Value -> Value
  ; Function that evaluates the binary operation being applied and evaluates the opaeration to a Real
(define (getBinop [op : Symbol] [l : Value] [r : Value]) : Value
  (match op
    [`+ (cond
          [(and (NumV? l) (NumV? r)) (NumV (+ (NumV-n l) (NumV-n r)))]
          [else (error 'getBinop "JILI Wrong Type For This Operation")])]
    [`* (cond
          [(and (NumV? l) (NumV? r)) (NumV (* (NumV-n l) (NumV-n r)))]
          [else (error 'getBinop "JILI Wrong Type For This Operation")])]
    [`- (cond
          [(and (NumV? l) (NumV? r)) (NumV (- (NumV-n l) (NumV-n r)))]
          [else (error 'getBinop "JILI Wrong Type For This Operation")])]
    [`/ (cond
          [(and (NumV? l) (NumV? r)) (cond
                                       [(equal? r (NumV 0)) (error 'getBinop "JILI Division By Zero")]
                                       [else (NumV (/ (NumV-n l) (NumV-n r)))])]
          [else (error 'getBinop "JILI Wrong Type For This Operation")])]
    [`equal? (cond
               [(and (BoolV? l) (BoolV? r)) (BoolV (equal? (BoolV-n l) (BoolV-n r)))]
               [(and (NumV? l) (NumV? r)) (BoolV (equal? (NumV-n l) (NumV-n r)))]
               [(and (StringV? l) (StringV? r)) (BoolV (equal? (StringV-s l) (StringV-s r)))]
               [else (BoolV false)])]
    [`<= (cond
           [(and (NumV? l) (NumV? r)) (BoolV (<= (NumV-n l) (NumV-n r)))]
           [else (error 'getBinop "JILI Wrong Type For This Operation")])]
    [else (error 'getBinop "JILI Unexpected Symbol")]))



  ; checkDuplicates (Listof Symbol) -> Boolean
  ; Checks the list of symbols for duplicates returning true if no duplicates exist
(define (checkDuplicates [vars : (Listof Symbol)]) : Boolean
  (cond
    [(empty? vars) true]
    [(member (first vars) (rest vars)) false]
    [(member (first vars) `(if var in =>)) false]
    [else (checkDuplicates (rest vars))]))



  ; getSymbols Env -> (Listof Symbol)
  ; It grabs the symbols from an env and puts it in a list
(define (getSymbols [env : Env]) : (Listof Symbol)
  (match env
    [`() `()]
    [(cons f r) (cons (Binding-key f) (getSymbols r))]))



  ; getArgs: (Listof s-expression) -> (Listof ExprC)
  ; It parses a list of var s-expressons into a list of ExprCs by parsing each of the bodies
(define (getArgs [vars : (Listof Sexp)]) : (Listof ExprC)
  (match vars
    [`() `()]
    [(cons f r) (match f
                  [(list id `= body) (cons (parse body) (getArgs r))]
                  [other (error 'getIds "JILI id doesnt match")])]))



  ; getVars: (Listof s-expression) -> (Listof Symbol)
  ; It parses a list of var s-expressons grabbing only the id symbols and inserting them into a list
(define (getVars [vars : (Listof Sexp)]) : (Listof Symbol)
  (match vars
    [`() `()]
    [(cons f r) (match f
                  [(list (? symbol? id) `= body) (cons id (getVars r))]
                  [other (error 'getIds "JILI id doesnt match")])]))



;Testing Suite================================================================================
  ;Testing serialize (Aryan)
(check-equal? (serialize (StringV "hi")) "\"hi\"")
(check-equal? (serialize (NumV 1)) "1")
(check-equal? (serialize (StringV "yoyo")) "\"yoyo\"")
(check-equal? (serialize (StringV "how you been today?")) "\"how you been today?\"")
(check-equal? (serialize (BoolV #t)) "true")
(check-equal? (serialize (BoolV #f)) "false")
(check-equal? (serialize (CloV  (list `x)
                                (LamC '(x) (LamC '(y) (AppC (IdC '+) (list(IdC 'x) (IdC 'y)))))
                                (list (Binding `x (NumV 1))))) "#<procedure>")
(check-equal? (serialize (OpV `+)) "#<primop>")



  ;Testing Interp
(check-equal? (interp (NumC 1) `()) (NumV 1))
(check-equal? (interp (IdC `x) (list (Binding `x (NumV 1)))) (NumV 1))
(check-equal? (interp (StringC "x") topEnv) (StringV "x"))
(check-equal? (interp (LamC (list `x) (AppC (IdC `+) (list (IdC `x) (NumC 1)))) (list (Binding `x (NumV 1))))
              (CloV `(x) (AppC (IdC `+) (list (IdC `x) (NumC 1))) (list (Binding `x (NumV 1)))))
(check-equal? (interp (ifC (IdC `true) (IdC `true) (IdC `false)) topEnv) (BoolV true))
(check-equal? (interp (ifC (IdC `false) (IdC `true) (IdC `false)) topEnv) (BoolV false))
(check-equal? (interp (ifC (IdC `true) (AppC (IdC `+) (list (NumC 1) (NumC 2))) (IdC `false)) topEnv) (NumV 3))
(check-equal? (interp (ifC (IdC `false) (IdC `false) (AppC (IdC `+) (list (NumC 1) (NumC 2)))) topEnv) (NumV 3))
(check-exn (regexp (regexp-quote "JILI Improper ifC"))
           (lambda () (interp (ifC (IdC `+) (IdC `false) (AppC (IdC `+) (list (NumC 1) (NumC 2)))) topEnv)))
(check-exn (regexp (regexp-quote "JILI Improper ifC"))
           (lambda () (interp (ifC (AppC (IdC `+) (list (NumC 4) (NumC 3))) (NumC 1) (NumC 2)) topEnv)))
(check-equal? (interp (AppC (IdC `+) (list (NumC 1) (NumC 2)))topEnv) (NumV 3))
(check-equal? (interp (AppC (LamC (list `x) (AppC (IdC `+) (list (IdC `x) (NumC 1)))) (list (NumC 1))) topEnv) (NumV 2))
(check-exn (regexp (regexp-quote "JILI bad OpV"))
           (lambda () (interp (AppC (IdC `+) (list (NumC 1))) topEnv)))
(check-exn (regexp (regexp-quote "JILI user-error (StringC \"1234\")"))
           (lambda () (top-interp '(+ 4 (error "1234")))))



  ;Testing top-interp
(check-equal? (top-interp '{+ 1 2}) "3")
(check-equal? (top-interp '{* 1 2}) "2")
(check-equal? (top-interp '{- 1 2}) "-1")
(check-equal? (top-interp '{/ 1 2}) "1/2")
(check-equal? (top-interp '{{x => {+ x 2}} 1}) "3")
(check-equal? (top-interp '{{x => {* x 2}} 1}) "2")
(check-equal? (top-interp '{{x => {/ x 2}} 1}) "1/2")
(check-equal? (top-interp '{{x => {- x 2}} 1}) "-1")
(check-equal? (top-interp '{{x y => {- x y}} 1 1}) "0")
(check-equal? (top-interp (quote (=> 9))) "#<procedure>")
(check-equal? (top-interp (quote ((minus => (minus 8 5)) (a b => (+ a (* -1 b)))))) "3")
(check-equal? (top-interp (quote ((seven => (seven))
                                  ((minus => (=> (minus (+ 3 10) (* 2 3))))
                                   (x y => (+ x (* -1 y))))))) "7")
(check-exn (regexp (regexp-quote "JILI Improper AppC"))
           (lambda () (top-interp '(3 4 5))))

  ;Testing Parse
(check-equal? (parse 1) (NumC 1))
(check-equal? (parse `x) (IdC `x))
(check-equal? (parse `+) (IdC `+))
(check-equal? (parse `-) (IdC `-))
(check-equal? (parse `*) (IdC `*))
(check-equal? (parse `/) (IdC `/))
(check-equal? (parse `<=) (IdC `<=))
(check-equal? (parse `true) (IdC `true))
(check-equal? (parse `false) (IdC `false))
(check-equal? (parse `equal?) (IdC `equal?))
(check-equal? (parse "a") (StringC "a"))
(check-equal? (parse `{if true 1 false}) (ifC (IdC `true) (NumC 1) (IdC `false)))
(check-equal? (parse '{+ 1 2}) (AppC (IdC `+) (list (NumC 1) (NumC 2))))
(check-equal? (parse '{x => {+ x 2}}) (LamC (list `x) (AppC (IdC `+) (list (IdC `x) (NumC 2)))))
(check-equal? (parse '{x y => {+ x y}}) (LamC (list `x `y) (AppC (IdC `+) (list (IdC `x) (IdC `y)))))
(check-equal? (parse '{{x => {+ x 2}} 1})
              (AppC (LamC (list `x) (AppC (IdC `+) (list (IdC `x) (NumC 2)))) (list (NumC 1))))
(check-equal? (parse '{var
                       {[z = {+ 9 14}]
                        [y = 98]}
                       in
                       {+ z y}})
              (AppC (LamC (list `z `y) (AppC (IdC `+) (list (IdC `z) (IdC `y))))
                    (list (AppC (IdC `+) (list (NumC 9) (NumC 14))) (NumC 98))))
(check-exn (regexp (regexp-quote "JILI Empty S-expression"))
           (lambda () (parse `{})))
(check-exn (regexp (regexp-quote "JILI Unexpected symbol"))
           (lambda () (parse `if)))
(check-exn (regexp (regexp-quote "JILI Unexpected symbol"))
           (lambda () (parse `{if 1 2})))
(check-exn (regexp (regexp-quote "JILI Duplicate arg symbols"))
           (lambda () (parse '(x x => 3))))
(check-exn (regexp (regexp-quote "JILI Arguments Have Improper Format"))
           (lambda () (parse '(3 4 5 => 6))))
(check-exn (regexp (regexp-quote "JILI Duplicates"))
           (lambda () (parse '(var ((z = (=> 3)) (z = 9)) in (z)))))
(check-exn (regexp (regexp-quote "JILI Duplicate arg symbols"))
           (lambda () (parse '{in => {+ 1 2}})))



;Testing Helper Funcs================================================================================
  ;Testing extend-env
(check-equal? (extend-env (list (NumV 1)) (list `x) `()) (list (Binding `x (NumV 1))))
(check-equal? (extend-env (list (NumV 2)) (list `x) (list (Binding `x (NumV 1))))
              (list (Binding `x (NumV 2)) (Binding `x (NumV 1))))
(check-equal? (extend-env (list (NumV 2) (NumV 3)) (list `x `y) `()) (list (Binding `x (NumV 2)) (Binding `y (NumV 3))))
(check-equal? (extend-env (list (StringV "bruh") (NumV 3)) (list `x `y) `())
              (list (Binding 'x (StringV "bruh")) (Binding 'y (NumV 3))))
(check-equal? (extend-env (list (StringV "bruh") (StringV "this is a test.")) (list `x `y) `())
              (list (Binding 'x (StringV "bruh")) (Binding 'y (StringV "this is a test."))))
(check-equal? (extend-env `() (list `x) `()) `()) 
(check-exn (regexp (regexp-quote "JILI Too many args"))
           (lambda () (extend-env (list (StringV "bruh") (StringV "this is a test.")) (list `x) `())))
(check-exn (regexp (regexp-quote "JILI Too many args"))
           (lambda () (extend-env (list (NumV 1))`(s a) `())))



  ;Testing lookup
(check-equal? (lookup `true topEnv) (BoolV true))
(check-equal? (lookup `x (list(Binding `x (NumV 3)))) (NumV 3))
(check-equal? (lookup `x (list(Binding `x (StringV "yoyo"))
                              (Binding `y (NumV 1))))
              (StringV "yoyo"))
(check-exn (regexp (regexp-quote "JILI name not found"))
           (lambda () (lookup `s `())))



  ;Testing getSymbols
(check-equal? (getSymbols `()) `())
(check-equal? (getSymbols topEnv) `(+ - / * true false <= equal? error))



  ;Testing getArgs
(check-equal? (getArgs `()) `())
(check-equal? (getArgs `([z = {+ 9 14}]))
              (list (AppC (IdC `+) (list (NumC 9) (NumC 14)))))
(check-equal? (getArgs `([z = {+ 9 14}] [y = 98]))
              (list (AppC (IdC `+) (list (NumC 9) (NumC 14))) (NumC 98)))
(check-exn (regexp (regexp-quote "JILI id doesnt match"))
           (lambda () (getArgs `([===]))))



  ;Testing getVars
(check-equal? (getVars `()) `())
(check-equal? (getVars `([z = {+ 9 14}])) (list `z))
(check-equal? (getVars `([z = {+ 9 14}] [y = 98])) (list `z `y))
(check-exn (regexp (regexp-quote "JILI id doesnt match"))
           (lambda () (getVars `([===]))))



 ;Testing checkDuplicates
(check-equal? (checkDuplicates '(x y)) true)
(check-equal? (checkDuplicates '(x x)) false)



  ;Testing checkArgs
(check-equal? (checkArgs (quote (=> 9)))
              (LamC '() (NumC 9)))
(check-equal? (checkArgs `{x => {+ x 1}})
             (LamC (list 'x) (AppC (IdC `+) (list (IdC `x)(NumC 1)))))
(check-equal? (checkArgs `{x y => {+ x y}})
             (LamC '(x y) (AppC (IdC '+) (list (IdC 'x) (IdC 'y)))))
(check-equal? (checkArgs '[z = {+ 9 14}])
              (LamC `(z) (AppC (IdC `+) (list (NumC 9) (NumC 14)))))
(check-equal? (checkArgs `{{y => {+ 1 y}} 2})
             (AppC (LamC '(y) (AppC (IdC '+) (list (NumC 1) (IdC 'y)))) (list (NumC 2))))
(check-equal? (checkArgs '{var
                       {[z = {+ 9 14}]
                        [y = 98]}
                       in
                       {+ z y}})
              (AppC (LamC (list `z `y) (AppC (IdC `+) (list (IdC `z) (IdC `y))))
                    (list (AppC (IdC `+) (list (NumC 9) (NumC 14))) (NumC 98))))
(check-exn (regexp (regexp-quote "JILI No Arguments"))
           (lambda () (checkArgs `())))



  ;Testing GetBinop
(check-equal? (getBinop `+ (NumV 1) (NumV 2)) (NumV 3))
(check-equal? (getBinop `* (NumV 1) (NumV 2)) (NumV 2))
(check-equal? (getBinop `- (NumV 1) (NumV 10)) (NumV -9))
(check-equal? (getBinop `/ (NumV 1) (NumV 10)) (NumV 1/10))
(check-exn (regexp (regexp-quote "JILI Wrong Type For This Operation"))
           (lambda () (getBinop `+ (BoolV true) (BoolV true))))
(check-exn (regexp (regexp-quote "JILI Wrong Type For This Operation"))
           (lambda () (getBinop `* (BoolV true) (BoolV true))))
(check-exn (regexp (regexp-quote "JILI Wrong Type For This Operation"))
           (lambda () (getBinop `- (BoolV true) (BoolV true))))
(check-exn (regexp (regexp-quote "JILI Wrong Type For This Operation"))
           (lambda () (getBinop `/ (BoolV true) (BoolV true))))
(check-exn (regexp (regexp-quote "JILI Division By Zero"))
           (lambda () (getBinop `/ (NumV 1) (NumV 0))))
(check-equal? (getBinop `equal? (NumV 2) (NumV 2)) (BoolV #t))
(check-equal? (getBinop `equal? (NumV 1) (NumV 2)) (BoolV #f))
(check-equal? (getBinop `equal? (BoolV #t) (BoolV #t)) (BoolV #t))
(check-equal? (getBinop `equal? (BoolV #t) (BoolV #f)) (BoolV #f))
(check-equal? (getBinop `equal? (StringV "a") (StringV "a")) (BoolV #t))
(check-equal? (getBinop `equal? (StringV "a") (StringV "b")) (BoolV #f))
(check-equal? (getBinop `equal? (NumV 1) (StringV "b")) (BoolV #f))
(check-equal? (getBinop `<= (NumV 1) (NumV 1)) (BoolV #t))
(check-equal? (getBinop `<= (NumV 11) (NumV 10)) (BoolV #f))
(check-equal? (getBinop `<= (NumV 1) (NumV 10)) (BoolV #t))
(check-exn (regexp (regexp-quote "JILI Wrong Type For This Operation"))
           (lambda () (getBinop `<= (BoolV true) (BoolV true))))
(check-exn (regexp (regexp-quote "JILI Wrong Type For This Operation"))
           (lambda () (getBinop `<= (StringV "a") (StringV "a"))))
(check-exn (regexp (regexp-quote "JILI Unexpected Symbol"))
           (lambda () (getBinop `! (NumV 1) (NumV 1))))
#;(parse (quote ((seven => (seven))
                                  ((minus => (=> (minus (+ 3 10) (* 2 3))))
                                   (x y => (+ x (* -1 y)))))))
#;(parse `{{f 4} {x => 44}})