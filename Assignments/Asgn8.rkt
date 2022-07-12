#lang typed/racket
(require typed/rackunit)
;Fully implemented 0 Cases Failing!

;Structure Definitions
  ;ExprCs
(define-type ExprC (U NumC IdC StringC LamC AppC ifC RecC))
(struct NumC ([n : Real]) #:transparent)
(struct IdC ([s : Symbol]) #:transparent)
(struct StringC ([s : String]) #:transparent)
(struct LamC ([args : (Listof Symbol)] [argT : (Listof Type)] [body : ExprC]) #:transparent)
(struct AppC ([fun : ExprC] [args : (Listof ExprC)]) #:transparent)
(struct ifC ([if : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
(struct RecC ([name : Symbol] [args : (Listof Symbol)] [funDef : ExprC]
                              [argT : (Listof Type)] [retTy : Type] [body : ExprC]) #:transparent)



  ;Types
(define-type Type (U NumT FunT BoolT StringT))
(struct NumT () #:transparent)
(struct BoolT () #:transparent)
(struct StringT () #:transparent)
(struct FunT ([arg : (Listof Type)] [ret : Type]) #:transparent)


  ;Bindings
(struct Binding ([key : Symbol] [value : (Boxof Value)]) #:transparent)
(define-type Env (Listof Binding))
(struct TBinding ([key : Symbol][ty : Type]) #:transparent)
(define-type Tenv (Listof TBinding))



  ;Values
(define-type Value (U NumV BoolV CloV StringV OpV))
(struct NumV ([n : Real]) #:transparent)
(struct BoolV ([n : Boolean]) #:transparent)
(struct StringV ([s : String]) #:transparent)
(struct CloV ([args : (Listof Symbol)] [body : ExprC] [env : Env]) #:transparent)
(struct OpV ([op : Symbol]) #:transparent)
(define topEnv (list (Binding `+ (box (OpV `+)))
                     (Binding `- (box (OpV `-)))
                     (Binding `/ (box (OpV `/)))
                     (Binding `* (box (OpV `*)))
                     (Binding `true (box (BoolV true)))
                     (Binding `false (box (BoolV false)))
                     (Binding `<= (box (OpV `<=)))
                     (Binding `num-eq? (box (OpV `num-eq?)))
                     (Binding `str-eq? (box (OpV `str-eq?)))
                     (Binding `substring (box (OpV `substring)))))
(define base-tenv (list (TBinding `+ (FunT (list (NumT) (NumT)) (NumT)))
                     (TBinding `- (FunT (list (NumT) (NumT)) (NumT)))
                     (TBinding `/ (FunT (list (NumT) (NumT)) (NumT)))
                     (TBinding `* (FunT (list (NumT) (NumT)) (NumT)))
                     (TBinding `true (BoolT))
                     (TBinding `false (BoolT))
                     (TBinding `<= (FunT (list (NumT) (NumT)) (BoolT)))
                     (TBinding `num-eq?  (FunT (list (NumT) (NumT)) (BoolT)))
                     (TBinding `str-eq? (FunT (list (StringT) (StringT)) (BoolT)))
                     (TBinding `substring (FunT (list (StringT) (NumT) (NumT)) (StringT)))))



;Parsers================================================================================
  ; parse: s-expression -> ExprC
  ; It parses an s-expresson into an ExprC type if not then a JILI error is raised
(define (parse [s : Sexp]) : ExprC
  (match s
    [`() (error 'parse "JILI Empty S-expression")]
    [(? real? r) (NumC r)]
    [(? string? str) (StringC str)]
    [(list `if if then else) (ifC (parse if) (parse then) (parse else))]
    [(list l ...)
     (match l                                  
       [(list (list (? symbol? id) `: ty) ... `=> body)
        (define args (cast id (Listof Sexp)))
        (define type (cast ty (Listof Sexp)))
        (if
         (checkDuplicates (cast args (Listof Symbol)))
         (LamC (cast args (Listof Symbol)) (map (lambda ([x : Sexp]) (parseType x)) type) (parse body))
         (error 'parse "JILI Duplicate arg symbols"))]
       [(list `var (list (list (? symbol? id) `: ty `= bodies) ...) `in body)
        (define type (cast ty (Listof Sexp)))
        (define ids (cast id (Listof Symbol)))
        (if (checkDuplicates ids)
            (AppC (LamC ids
                        (map (lambda ([x : Sexp]) (parseType x)) type)
                        (parse body))
                  (map (lambda ([x : Sexp]) (parse x)) (cast bodies (Listof Sexp))))
            (error 'parse "JILI Duplicates"))]
       [(list `rec (list (? symbol? name) `=
                         (list (list (? symbol? id) `: ty) ... `: retTy `=> funDef)) `in body)
        (RecC name (cast id (Listof Symbol)) (parse funDef)
              (map (lambda ([x : Sexp]) (parseType x)) (cast ty (Listof Sexp)))
              (parseType retTy)
              (parse body))]
       [(list l ...)
        (AppC (parse (first l))
              (cast (map (lambda ([x : Sexp]) (parse x)) (cast (rest l) (Listof Sexp))) (Listof ExprC)))])]
    [(? symbol? s) (cond
                     [(member s `(if var in =>)) (error 'parse "JILI Unexpected symbol ~e" s)]
                     [else (IdC s)])]))



  ; parseType: s-expression -> Type
  ; Function that parses S-expressions into Types
(define (parseType [s : Sexp]) : Type
  (match s
    [`num (NumT)]
    [`str (StringT)]
    [`bool (BoolT)]
    [(list t1 ... `-> t2) (FunT (map (lambda ([x : Sexp]) (parseType x)) (cast t1 (Listof Sexp))) (parseType t2))]
    [other (error 'parseType "JILI Invalid type ~e" s)]))



;Interps================================================================================
  ; interp: ExprC, (Listof FunDefC) -> Real
  ; A function that referenceds a Listof FunDefC's and interprets an ExprC into a real
(define (interp [exp : ExprC] [env : Env]) : Value
  (match exp
    [(NumC n) (NumV n)]
    [(IdC s) (unbox (lookup s env))]
    [(StringC str) (StringV str)]
    [(LamC args argT body) (CloV args body env)]
    [(ifC if then else) (cond
                          [(BoolV? (interp if env))
                           (match (BoolV-n (cast (interp if env) BoolV))
                             [#t (interp then env)]
                             [#f (interp else env)])]
                          [else (error 'inter "JILI Improper ifC")])]
    [(RecC name args funDef argT retT body)
     (define extended-env (extend-env (list (StringV "bogus")) (list name) env))
     (begin (set-box! (lookup name extended-env) (CloV args funDef extended-env)) (interp body extended-env))]
    [(AppC fun arguments)
     (define fd (interp fun env))
     (match fd
       [(OpV op) (cond
                   [(equal? 2 (length arguments))
                    (getBinop op (interp (first arguments) env) (interp (second arguments) env))]
                   [(and (equal? 3 (length arguments)) (equal? `substring op))
                    (define str (interp (first arguments) env))
                    (define begin (interp (second arguments) env))
                    (define end (interp (third arguments) env))
                    (cond
                      [(and (StringV? str)
                            (NumV? begin)
                            (NumV? end)
                            (integer? (NumV-n (cast begin NumV))) (integer? (NumV-n (cast end NumV))))
                       (StringV (substring
                                 (StringV-s (cast str StringV))
                                 (cast (NumV-n (cast begin NumV)) Integer)
                                 (cast (NumV-n (cast end NumV)) Integer)))]
                      [else (error 'getBinop "JILI Wrong Type For This Operation")])]
                   [else (error 'interp "JILI bad OpV")])]
       [(CloV args body clovEnv)
        (interp body (extend-env (map (lambda ([x : ExprC]) (interp x env)) arguments) args clovEnv))])]))



  ; top-interp: s-expression -> Real 
  ; Function that interprets at the top level and reads in the raw s-expressions to evaluate to Reals
(define (top-interp [fun-sexps : Sexp]) : String
  (begin (type-check (parse fun-sexps) base-tenv) (serialize (interp (parse fun-sexps) topEnv))))



;TYPE CHECKER=============================================================
  ; type-check: ExprC, Tenv -> Type
  ; Function that checks the type of an ExprC
(define (type-check [expr : ExprC] [tenv : Tenv]) : Type
  (match expr
    [(NumC n) (NumT)]
    [(StringC str) (StringT)]
    [(IdC s) (lookupT s tenv)]
    [(ifC if then else) (let ([tht (type-check then tenv)]
               [elt (type-check else tenv)])
           (cond 
             [(not (BoolT? (type-check if tenv))) 
              (error 'type-check "JILI not boolean in test clause of if")]
             [(not (equal? tht elt)) 
              (error 'type-check "JILI type mismatch in if")]
             [else tht]))]
    [(AppC fun args)
     (define funType (type-check fun tenv))                  
     (define argType (map (lambda ([x : ExprC]) (type-check x tenv)) args))
              (cond
                [(not (FunT? funType))
                 (error 'type-check "JILI not a function")]
                [(not (equal? (FunT-arg funType) argType))
                 (error 'type-check "JILI app arg mismatch")]
                [else (FunT-ret (cast funType FunT))])]
    [(LamC args argT body) (FunT argT (type-check body (extend-Tenv argT args tenv)))]
    [(RecC name args funDef argT retT body)
     (define extended-env
       (extend-Tenv (list (FunT argT retT)) (list name) tenv))
     (define extended-env2
       (extend-Tenv argT args extended-env))
     (cond
       [(not (equal? retT (type-check funDef
                              (extend-Tenv
                               argT args
                               extended-env))))
        (error 'type-check "JILI funDef return type not correct")]
       [else (type-check body
                              extended-env)])]))



;Envrionment and new Asgn 5 Stuff================================================================================
  ; lookup: Symbol, Env -> Value
  ; A function that looksup the symbol in the Env and gets the binded Value
(define (lookup [for : Symbol] [env : Env]) : (Boxof Value)
  (cond
    [(empty? env) (error 'lookup "JILI name not found: ~e" for)]
    [else (cond
            [(equal? for (Binding-key (first env)))
             (Binding-value (first env))]
            [else (lookup for (rest env))])]))



  ; lookupT: Symbol, Env -> Value
  ; A function that looksup the symbol in the Env and gets the binded Value
(define (lookupT [for : Symbol] [env : Tenv]) : Type
  (cond
    [(empty? env) (error 'lookup "JILI name not found: ~e" for)]
    [else (cond
            [(equal? for (TBinding-key (first env)))
             (TBinding-ty (first env))]
            [else (lookupT for (rest env))])]))



  ; extend-env: (Listof ExprC), (Listof Symbol), Env-> Env
  ; A function that takes an exising Env and adds new bindings created from the vals and args
(define (extend-env [vals : (Listof Value)] [args : (Listof Symbol)] [curEnv : Env]) : Env
  (match vals
    [`() curEnv]
    [else (cond
            [(equal? (length vals) (length args)) (cons (Binding (first args) (box (first vals)))
                                                           (extend-env (rest vals) (rest args) curEnv))]
            [else (error 'extend-env "JILI Too many args")])]))



  ; extend-Tenv: (Listof ExprC), (Listof Symbol), Env-> Env
  ; A function that takes an exising Env and adds new bindings created from the vals and args
(define (extend-Tenv [types : (Listof Type)] [args : (Listof Symbol)] [curEnv : Tenv]) : Tenv
  (match types
    [`() curEnv]
    [else (cond
            [(equal? (length types) (length args)) (cons (TBinding (first args) (first types))
                                                           (extend-Tenv (rest types) (rest args) curEnv))]
            [else (error 'extend-Tenv "JILI Too many args")])]))



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
    [`num-eq? (if (and (NumV? l) (NumV? r))
                  (BoolV (equal? (NumV-n l) (NumV-n r)))
                  (error 'getBinop "JILI Wrong Type For This Operation"))]
    [`str-eq? (if (and (StringV? l) (StringV? r))
                  (BoolV (equal? (StringV-s l) (StringV-s r)))
                  (error 'getBinop "JILI Wrong Type For This Operation"))]
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


;Testing Suite================================================================================
  ;Testing Type Parser
(check-equal? (parseType `num) (NumT))
(check-equal? (parseType `str) (StringT))
(check-equal? (parseType `bool) (BoolT))
(check-equal? (parseType `{num -> num}) (FunT (list (NumT)) (NumT)))
(check-equal? (parseType `{num num -> num}) (FunT (list (NumT) (NumT)) (NumT)))
(check-equal? (parseType `{num -> {num -> num}}) (FunT (list (NumT)) (FunT (list (NumT)) (NumT))))
(check-exn (regexp (regexp-quote "JILI Invalid type"))
           (lambda () (parseType `{num -> 14})))



  ;Testing Type Checker
(check-equal? (type-check (NumC 1) base-tenv) (NumT))
(check-equal? (type-check (StringC "1") base-tenv) (StringT))
(check-equal? (type-check (ifC (IdC 'true) (IdC 'true) (IdC 'false)) base-tenv) (BoolT))
(check-equal? (type-check (AppC (IdC `+) (list (NumC 1) (NumC 1))) base-tenv) (NumT))
(check-equal? (type-check (parse '(rec (a = ((c : num) : num => 12)) in 13)) base-tenv) (NumT))
(check-equal? (type-check (parse '(rec (a = ((c : num) : num => 12)) in a)) base-tenv) (FunT (list (NumT)) (NumT)))
(check-equal? (type-check (parse '(rec (a = ((c : num) : num => c)) in a)) base-tenv) (FunT (list (NumT)) (NumT)))
(check-equal? (type-check (parse '(rec (a = ((c : num) : num => {a 1})) in {a 1})) base-tenv) (NumT))
(check-equal? (type-check (parse '(rec (a = ((c : num) : num => {a 1})) in "abc")) base-tenv) (StringT))
(check-equal? (type-check (LamC (list 'x) (list (NumT)) (AppC (IdC `+) (list (IdC `x) (NumC 1)))) base-tenv)
              (FunT (list (NumT)) (NumT)))
(check-exn (regexp (regexp-quote "JILI not boolean in test clause of if"))
           (lambda () (type-check (ifC (NumC 8) (IdC 'false) (IdC 'false)) base-tenv)))
(check-exn (regexp (regexp-quote "JILI type mismatch in if"))
           (lambda () (type-check (ifC (IdC `false) (IdC 'false) (NumC 8)) base-tenv)))
(check-exn (regexp (regexp-quote "JILI app arg mismatch"))
           (lambda () (type-check (AppC (IdC `+) (list (IdC `false) (NumC 1))) base-tenv)))
(check-exn (regexp (regexp-quote "JILI not a function"))
           (lambda () (type-check (AppC (IdC `false) (list (NumC 3) (NumC 1))) base-tenv)))
(check-exn (regexp (regexp-quote "JILI app arg mismatch"))
           (lambda () (type-check (AppC (IdC `+) (list (StringC "3") (NumC 1))) base-tenv)))
(check-exn (regexp (regexp-quote "JILI funDef return type not correct"))
           (lambda () (type-check (parse '(rec (a = ((c : num) : num => "abc")) in 13)) base-tenv)))
(check-exn (regexp (regexp-quote "JILI app arg mismatch"))
           (lambda () (type-check (parse '(rec (a = ((c : num) : num => {a 1})) in {+ 1 a})) base-tenv)))



  ;Testing serialize
(check-equal? (serialize (StringV "hi")) "\"hi\"")
(check-equal? (serialize (NumV 1)) "1")
(check-equal? (serialize (StringV "yoyo")) "\"yoyo\"")
(check-equal? (serialize (StringV "how you been today?")) "\"how you been today?\"")
(check-equal? (serialize (BoolV #t)) "true")
(check-equal? (serialize (BoolV #f)) "false")
(check-equal? (serialize (CloV  (list `x)
                                (LamC '(x) (list (NumT)) (LamC '(y) (list (NumT))
                                                               (AppC (IdC '+) (list(IdC 'x) (IdC 'y)))))
                                (list (Binding `x (box (NumV 1)))))) "#<procedure>")
(check-equal? (serialize (OpV `+)) "#<primop>")



  ;Testing Interp
(check-equal? (interp (NumC 1) `()) (NumV 1))
(check-equal? (interp (IdC `x) (list (Binding `x (box (NumV 1))))) (NumV 1))
(check-equal? (interp (StringC "x") topEnv) (StringV "x"))
(check-equal? (interp (LamC (list `x) (list (NumT)) (AppC (IdC `+) (list (IdC `x) (NumC 1))))
                      (list (Binding `x (box (NumV 1)))))
              (CloV `(x) (AppC (IdC `+) (list (IdC `x) (NumC 1))) (list (Binding `x (box (NumV 1))))))
(check-equal? (interp (ifC (IdC `true) (IdC `true) (IdC `false)) topEnv) (BoolV true))
(check-equal? (interp (ifC (IdC `false) (IdC `true) (IdC `false)) topEnv) (BoolV false))
(check-equal? (interp (ifC (IdC `true) (AppC (IdC `+) (list (NumC 1) (NumC 2))) (IdC `false)) topEnv) (NumV 3))
(check-equal? (interp (ifC (IdC `false) (IdC `false) (AppC (IdC `+) (list (NumC 1) (NumC 2)))) topEnv) (NumV 3))
(check-exn (regexp (regexp-quote "JILI Improper ifC"))
           (lambda () (interp (ifC (IdC `+) (IdC `false) (AppC (IdC `+) (list (NumC 1) (NumC 2)))) topEnv)))
(check-exn (regexp (regexp-quote "JILI Improper ifC"))
           (lambda () (interp (ifC (AppC (IdC `+) (list (NumC 4) (NumC 3))) (NumC 1) (NumC 2)) topEnv)))
(check-equal? (interp (AppC (IdC `+) (list (NumC 1) (NumC 2)))topEnv) (NumV 3))
(check-equal? (interp (AppC (IdC `substring) (list (StringC "Eric") (NumC 0) (NumC 2)))topEnv) (StringV "Er"))
(check-equal? (interp (AppC (LamC (list `x) (list (NumT))
                                  (AppC (IdC `+) (list (IdC `x) (NumC 1)))) (list (NumC 1))) topEnv) (NumV 2))
(check-exn (regexp (regexp-quote "JILI bad OpV"))
           (lambda () (interp (AppC (IdC `+) (list (NumC 1))) topEnv)))
(check-exn (regexp (regexp-quote "JILI Wrong Type For This Operation"))
           (lambda () (interp (AppC (IdC `substring) (list (NumC 1) (NumC 1) (NumC 1))) topEnv)))



  ;Testing top-interp
(check-equal? (top-interp '{+ 1 2}) "3")
(check-equal? (top-interp '{* 1 2}) "2")
(check-equal? (top-interp '{- 1 2}) "-1")
(check-equal? (top-interp '{/ 1 2}) "1/2")
(check-equal? (top-interp `{rec {square-helper = {[n : num] : num =>
                       {if {<= n 0} 0 {+ n {square-helper {- n 2}}}}}}
     in
  {var {[square : {num -> num}  =
        {[n : num] => {square-helper {- {* 2 n} 1}}}]}
    in
    {square 13}}}) "169")



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
(check-equal? (parse '{[x : num] => {+ x 2}})
              (LamC (list `x) (list (NumT)) (AppC (IdC `+) (list (IdC `x) (NumC 2)))))
(check-equal? (parse `{[x : num] [y : num] => {+ x y}})
              (LamC (list `x `y) (list (NumT) (NumT)) (AppC (IdC `+) (list (IdC `x) (IdC `y)))))
(check-equal? (parse '{{[x : num] => {+ x 2}} 1})
              (AppC (LamC (list `x) (list (NumT)) (AppC (IdC `+) (list (IdC `x) (NumC 2)))) (list (NumC 1))))
(check-equal? (parse '{{[x : num] [y : num] => {+ x y}} 1 1})
              (AppC (LamC (list `x `y)
                          (list (NumT) (NumT))
                          (AppC (IdC `+) (list (IdC `x) (IdC `y))))
                    (list (NumC 1) (NumC 1))))
(check-equal? (parse '{var
                       {[z : num = 2]
                        [y : num = 98]}
                       in
                       {+ z y}})
              (AppC (LamC (list `z `y) (list (NumT) (NumT)) (AppC (IdC `+) (list (IdC `z) (IdC `y))))
                    (list (NumC 2) (NumC 98))))
(check-equal? (parse '{var
                       {[z : num = {+ 9 14}]
                        [y : num = 98]}
                       in
                       {+ z y}})
              (AppC (LamC (list `z `y) (list (NumT) (NumT)) (AppC (IdC `+) (list (IdC `z) (IdC `y))))
                    (list (AppC (IdC `+) (list (NumC 9) (NumC 14))) (NumC 98))))
(check-equal? (parse '{rec {square-helper = {[n : num] : num =>
                       {if {<= n 0} 0 {+ n {square-helper {- n 2}}}}}}
     in
  {var {[square : {num -> num}  =
        {[n : num] => {square-helper {- {* 2 n} 1}}}]}
    in
    {square 13}}})
              (RecC
 'square-helper
 '(n)
 (ifC
  (AppC
   (IdC '<=)
   (list (IdC 'n) (NumC 0)))
  (NumC 0)
  (AppC
   (IdC '+)
   (list
    (IdC 'n)
    (AppC
     (IdC 'square-helper)
     (list
      (AppC
       (IdC '-)
       (list
        (IdC 'n)
        (NumC 2))))))))
 (list (NumT))
 (NumT)
 (AppC
  (LamC
   '(square)
   (list
    (FunT (list (NumT)) (NumT)))
   (AppC
    (IdC 'square)
    (list (NumC 13))))
  (list
   (LamC
    '(n)
    (list (NumT))
    (AppC
     (IdC 'square-helper)
     (list
      (AppC
       (IdC '-)
       (list
        (AppC
         (IdC '*)
         (list (NumC 2) (IdC 'n)))
        (NumC 1))))))))))
(check-exn (regexp (regexp-quote "JILI Empty S-expression"))
           (lambda () (parse `{})))
(check-exn (regexp (regexp-quote "JILI Empty S-expression"))
           (lambda () (parse `{{}})))
(check-exn (regexp (regexp-quote "JILI Unexpected symbol"))
           (lambda () (parse `if)))
(check-exn (regexp (regexp-quote "JILI Unexpected symbol"))
           (lambda () (parse `{if 1 2})))
(check-exn (regexp (regexp-quote "JILI Duplicate arg symbols"))
           (lambda () (parse '([x : num] [x : num] => 3))))
(check-exn (regexp (regexp-quote "JILI Duplicate arg symbols"))
           (lambda () (parse '{[in : num] => {+ 1 2}})))
(check-exn (regexp (regexp-quote "JILI Duplicates"))
           (lambda () (parse '(var ((z : num = (=> 3)) (z : num = 9)) in (z)))))



;Testing Helper Funcs================================================================================
  ;Testing lookup
(check-equal? (lookup `true topEnv) (box (BoolV true)))
(check-equal? (lookup `x (list(Binding `x (box (NumV 3))))) (box (NumV 3)))
(check-equal? (lookup `x (list(Binding `x (box (StringV "yoyo")))
                              (Binding `y (box (NumV 1)))))
              (box (StringV "yoyo")))
(check-exn (regexp (regexp-quote "JILI name not found"))
           (lambda () (lookup `s `())))



  ;Testing lookupT
(check-equal? (lookupT `true base-tenv) (BoolT ))
(check-equal? (lookupT `<= base-tenv) (FunT (list (NumT) (NumT)) (BoolT)))
(check-equal? (lookupT `+ base-tenv)  (FunT (list (NumT) (NumT)) (NumT)))
(check-exn (regexp (regexp-quote "JILI name not found"))
           (lambda () (lookupT `s `())))



  ;Testing extend-env
(check-equal? (extend-env (list (NumV 1)) (list `x) `()) (list (Binding `x (box (NumV 1)))))
(check-equal? (extend-env (list (NumV 2)) (list `x) (list (Binding `x (box (NumV 1)))))
              (list (Binding `x (box (NumV 2))) (Binding `x (box (NumV 1)))))
(check-equal? (extend-env (list (NumV 2) (NumV 3)) (list `x `y) `())
              (list (Binding `x (box (NumV 2))) (Binding `y (box (NumV 3)))))
(check-equal? (extend-env (list (StringV "bruh") (NumV 3)) (list `x `y) `())
              (list (Binding 'x (box (StringV "bruh"))) (Binding 'y (box (NumV 3)))))
(check-equal? (extend-env (list (StringV "bruh") (StringV "this is a test.")) (list `x `y) `())
              (list (Binding 'x (box (StringV "bruh"))) (Binding 'y (box (StringV "this is a test.")))))
(check-equal? (extend-env `() (list `x) `()) `()) 
(check-exn (regexp (regexp-quote "JILI Too many args"))
           (lambda () (extend-env (list (StringV "bruh") (StringV "this is a test.")) (list `x) `())))
(check-exn (regexp (regexp-quote "JILI Too many args"))
           (lambda () (extend-env (list (NumV 1))`(s a) `())))



  ;Testing extend-Tenv
(check-equal?(extend-Tenv(list (BoolT)) (list `x) `())
             (list (TBinding 'x (BoolT))))
(check-equal?(extend-Tenv(list (BoolT) (NumT)) (list `x `n) `())
             (list (TBinding 'x (BoolT)) (TBinding 'n (NumT))))
(check-exn (regexp (regexp-quote "JILI Too many args"))
           (lambda () (extend-Tenv (list (BoolT)(NumT))`(s) `())))



  ;Testing checkDuplicates
(check-equal? (checkDuplicates '(x y)) true)
(check-equal? (checkDuplicates '(x x)) false)



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
(check-equal? (getBinop `num-eq? (NumV 2) (NumV 2)) (BoolV #t))
(check-equal? (getBinop `num-eq? (NumV 1) (NumV 2)) (BoolV #f))
(check-equal? (getBinop `str-eq? (StringV "a") (StringV "a")) (BoolV #t))
(check-equal? (getBinop `str-eq? (StringV "a") (StringV "b")) (BoolV #f))
(check-equal? (getBinop `<= (NumV 1) (NumV 1)) (BoolV #t))
(check-equal? (getBinop `<= (NumV 11) (NumV 10)) (BoolV #f))
(check-equal? (getBinop `<= (NumV 1) (NumV 10)) (BoolV #t))
(check-exn (regexp (regexp-quote "JILI Wrong Type For This Operation"))
           (lambda () (getBinop `<= (BoolV true) (BoolV true))))
(check-exn (regexp (regexp-quote "JILI Wrong Type For This Operation"))
           (lambda () (getBinop `<= (StringV "a") (StringV "a"))))
(check-exn (regexp (regexp-quote "JILI Unexpected Symbol"))
           (lambda () (getBinop `! (NumV 1) (NumV 1))))
(check-exn (regexp (regexp-quote "JILI Wrong Type For This Operation"))
           (lambda () (getBinop `num-eq? (BoolV #t) (BoolV #t))))
(check-exn (regexp (regexp-quote "JILI Wrong Type For This Operation"))
           (lambda () (getBinop `str-eq? (NumV 1) (NumV 2))))