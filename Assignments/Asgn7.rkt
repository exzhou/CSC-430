#lang typed/racket
(require typed/rackunit)
;Not implmented fully Failing 1 Cases.
;Notes / Tasks
 ;1 while Test
 ;2 inorder Test
;Structure Definitions============================================================
  ;Location
(struct Location ([loc : Integer]) #:transparent)
  ;V*S
(struct v*s ([val : Value] [sto : Store]) #:transparent)
  ;L*S
(struct l*s ([loc : Location] [sto : Store]) #:transparent)
  ;Store
(struct Storage ([loc : Location] [val : Value]) #:transparent)
(define-type Store (Listof Storage))


  ;ExprC
(define-type ExprC (U NumC IdC StringC LamC AppC ifC MutC))
(struct NumC ([n : Real]) #:transparent)
(struct IdC ([s : Symbol]) #:transparent)
(struct StringC ([s : String]) #:transparent)
(struct LamC ([args : (Listof Symbol)] [body : ExprC]) #:transparent)
(struct MutC ([arg : Symbol] [body : ExprC]) #:transparent)
(struct AppC ([fun : ExprC] [args : (Listof ExprC)]) #:transparent)
(struct ifC ([if : ExprC] [then : ExprC] [else : ExprC]) #:transparent)


  ;Binding
(struct Binding ([key : Symbol] [loc : Location]) #:transparent)
(define-type Env (Listof Binding))


  ;Value
(define-type Value (U NumV BoolV CloV StringV OpV ArrayV NullV))
(struct NumV ([n : Real]) #:transparent)
(struct BoolV ([n : Boolean]) #:transparent)
(struct StringV ([s : String]) #:transparent)
(struct CloV ([args : (Listof Symbol)] [body : ExprC] [env : Env]) #:transparent)
(struct OpV ([op : Symbol]) #:transparent)
(struct NullV ([n : Null]) #:transparent)
(struct ArrayV ([loc : Location] [size : Integer]) #:transparent)
(define topEnv (list (Binding `null (Location 0))
                     (Binding `+ (Location 1))
                     (Binding `- (Location 2))
                     (Binding `/ (Location 3))
                     (Binding `* (Location 4))
                     (Binding `true (Location 5))
                     (Binding `false (Location 6))
                     (Binding `<= (Location 7))
                     (Binding `equal? (Location 8))
                     (Binding `error (Location 9))
                     (Binding `array (Location 10))
                     (Binding `new-array (Location 11))
                     (Binding `aref (Location 12))
                     (Binding `aset! (Location 13))
                     (Binding `begin (Location 14))
                     (Binding `substring (Location 15))))
(define topStore (list (Storage (Location 0) (NullV null))
                       (Storage (Location 1) (OpV `+))
                       (Storage (Location 2) (OpV `-))
                       (Storage (Location 3) (OpV `/))
                       (Storage (Location 4) (OpV `*))
                       (Storage (Location 5) (BoolV true))
                       (Storage (Location 6) (BoolV false))
                       (Storage (Location 7) (OpV `<=))
                       (Storage (Location 8) (OpV `equal?))
                       (Storage (Location 9) (OpV `error))
                       (Storage (Location 10) (OpV `array))
                       (Storage (Location 11) (OpV `new-array))
                       (Storage (Location 12) (OpV `aref))
                       (Storage (Location 13) (OpV `aset!))
                       (Storage (Location 14) (OpV `begin))
                       (Storage (Location 15) (OpV `substring))))
(define Restricted `(if var in => :=))
(define while 
 '{var {[while = "bogus"]}
       in
       {begin {while := {guard func => (if {guard} {begin {func} {while guard func}} "Done")}}
              while}})


(define in-order
  '{arr i_size =>
          {var {[size = i_size] [res = true] [while = ,while]}             
               in
               {var {
                     [guard = {=> {<= 2 size}}]
                     [body = {=> (if {<= (aref arr {- size 2})
                                         (aref arr {- size 1})} (size := {- size 1}) (res := false))}]}
                    in
                    (begin (while guard body) res)
                    }}})
;1. Parser/Interps================================================================================
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
                     [(member s Restricted) (error 'parse "JILI Restricted symbol")]
                     [else (IdC s)])]))



  ; interp: ExprC, (Listof FunDefC) -> Real
  ; A function that referenceds a Listof FunDefC's and interprets an ExprC into a real
(define (interp [exp : ExprC] [env : Env] [sto : Store]) : v*s
  (match exp
    [(NumC n) (v*s (NumV n) sto)]
    [(StringC str) (v*s (StringV str) sto)]
    [(IdC s) (v*s (fetch (lookup s env) sto) sto)]                           
    [(LamC args body) (v*s (CloV args body env) sto)]
    [(MutC arg body)
     (define vStar (interp body env sto))
     (v*s (NullV null) (storeValue (v*s-val vStar) (lookup arg env) (v*s-sto vStar)))]
    [(ifC if then else)
     (define vStar (interp if env sto))
     (cond
       [(BoolV? (v*s-val vStar))
        (match (BoolV-n (cast (v*s-val vStar) BoolV))
          [#t (interp then env (v*s-sto vStar))]
          [#f (interp else env (v*s-sto vStar))])]
                          [else (error 'inter "JILI Improper ifC")])]
    [(AppC fun arguments)
     (define fd (interp fun env sto))
     (match (v*s-val fd)
       [(OpV op)
        (define vals (getVals arguments env (v*s-sto fd)))
        (cond
          [(equal? 2 (length arguments))
           (getBinop op
                     (v*s-val (first vals))
                     (v*s-val (second vals)) (v*s-sto (second vals)) env)]
          [(equal? 1 (length arguments))
           (cond
             [(equal? `error op) (error 'interp "JILI user-error ~v" (first arguments))]
             [(equal? `begin op) (multOp op (map (lambda ([x : v*s]) (v*s-val x)) vals) (v*s-sto (last vals)))]
             [(equal? `array op) (multOp op (map (lambda ([x : v*s]) (v*s-val x)) vals) (v*s-sto (last vals)))]
             [else (error 'interp "JILI bad OpV")])]
          [else (multOp op (map (lambda ([x : v*s]) (v*s-val x)) vals) (v*s-sto (last vals)))])]
       [(CloV args body clovEnv)
        (cond
          [(equal? (length args) (length arguments))
           (define vals (getVals arguments env (v*s-sto fd)))
           (if
            (empty? vals)
            (interp body (extend-env 0 args clovEnv) (storeValues (valsFromStars vals)
                                                                  (v*s-sto fd)))
            (interp body
                    (extend-env (length (v*s-sto (last vals))) args clovEnv)
                    (storeValues (map (lambda ([x : v*s]) (v*s-val x)) vals) (v*s-sto (last vals)))))]
          [else (error 'interp "JILI Improper AppC ~e ~e" arguments args)])]
       [other (error 'interp "JILI Improper AppC ~e ~e" arguments fun)])]))



  ; top-interp: s-expression -> String 
  ; Function that interprets at the top level and reads in the raw s-expressions to evaluate to Reals
(define (top-interp [fun-sexps : Sexp]) : String
  (serialize (v*s-val (interp (parse fun-sexps) topEnv topStore))))



;2. Asgn 7 Stuff=================================================================================
  ; multOp: Symbol, (Listof Value), Store -> v*s
  ; Function that evaluates the multi arg operation being applied and evaluates the opaeration to a v*s
(define (multOp [op : Symbol] [vals : (Listof Value)] [sto : Store]) : v*s
  (match op
    [`array (v*s (ArrayV (Location (length sto)) (length vals)) (storeValues vals sto))]
    [`begin (v*s (last vals) (storeValues vals sto))]
    [`substring (cond
               [(and (StringV? (first vals)) (NumV? (second vals)) (NumV? (third vals))
                     (integer? (NumV-n (cast (second vals) NumV))) (integer? (NumV-n (cast (third vals) NumV))))
                (v*s (StringV (substring
                               (StringV-s (cast (first vals) StringV))
                               (cast (NumV-n (cast (second vals) NumV)) Integer)
                               (cast (NumV-n (cast (third vals) NumV)) Integer))) sto)]
               [else (error 'getBinop "JILI Wrong Type For This Operation")])]
    [`aset! (match vals
              [(list (? ArrayV? arr) (? NumV? num) value)
               (cond
                 [(<= (ArrayV-size (cast arr ArrayV)) (NumV-n (cast num NumV)))
                  (error 'multOp "JILI Index out of bounds")]
                 [(> 0 (NumV-n (cast num NumV)))
                  (error 'multOp "JILI Index out of bounds")]
                 [(not (integer? (NumV-n (cast num NumV))))
                  (error 'multOp "JILI Index out of bounds")]
                 [else (v*s (NullV null)
                            (storeValue (last vals)
                                        (Location (cast (+ (Location-loc (ArrayV-loc arr)) (NumV-n num)) Integer))
                                        sto))])]
              [else (error 'multOp "JILI Wrong Type For This Operation")])]))



  ; getBinop: Symbol, Value, Value, Store -> v*s
  ; Function that evaluates the binary operation being applied and evaluates the opaeration to a v*s
(define (getBinop [op : Symbol] [l : Value] [r : Value] [sto : Store] [env : Env]) : v*s
  (match op
    [`+ (cond
          [(and (NumV? l) (NumV? r)) (v*s (NumV (+ (NumV-n l) (NumV-n r))) sto)]
          [else (error 'getBinop "JILI Wrong Type For This Operation ~e ~e" l r)])]
    [`* (cond
          [(and (NumV? l) (NumV? r)) (v*s (NumV (* (NumV-n l) (NumV-n r))) sto)]
          [else (error 'getBinop "JILI Wrong Type For This Operation")])]
    [`- (cond
          [(and (NumV? l) (NumV? r)) (v*s (NumV (- (NumV-n l) (NumV-n r))) sto)]
          [else (error 'getBinop "JILI Wrong Type For This Operation")])]
    [`/ (cond
          [(and (NumV? l) (NumV? r))
           (cond
             [(equal? r (NumV 0)) (error 'getBinop "JILI Division By Zero")]
             [else (v*s (NumV (/ (NumV-n l) (NumV-n r))) sto)])]
          [else (error 'getBinop "JILI Wrong Type For This Operation")])]
    [`equal? (cond
               [(and (BoolV? l) (BoolV? r))
                (v*s (BoolV (equal? (BoolV-n l) (BoolV-n r))) sto)]
               [(and (NumV? l) (NumV? r))
                (v*s (BoolV (equal? (NumV-n l) (NumV-n r))) sto)]
               [(and (StringV? l) (StringV? r))
                (v*s (BoolV (equal? (StringV-s l) (StringV-s r))) sto)]
               [(and (ArrayV? l) (ArrayV? r))
                (v*s (BoolV (and (equal? (ArrayV-loc l) (ArrayV-loc r)) (equal? (ArrayV-size l) (ArrayV-size r)))) sto)]
               [else (v*s (BoolV false) sto)])]
    [`new-array (cond
                  [(NumV? l) 
                    (cond
                      [(> 0 (NumV-n (cast l NumV)))
                       (error 'multOp "JILI Index out of bounds")]
                      [(not (integer? (NumV-n (cast l NumV))))
                       (error 'multOp "JILI Index out of bounds")]
                      [else (define allocated (allocate sto (cast (NumV-n l) Integer) r))
                            (v*s (ArrayV (l*s-loc allocated) (cast (NumV-n l) Integer)) (l*s-sto allocated))])]
                  [else (error 'getBinop "JILI Wrong Type For This Operation")])]
    [`aref (cond
             [(and (ArrayV? l) (NumV? r))
               (cond
                 [(<= (ArrayV-size (cast l ArrayV)) (NumV-n (cast r NumV)))
                  (error 'getBinop "JILI Index out of bounds")]
                 [(> 0 (NumV-n (cast r NumV)))
                  (error 'multOp "JILI Index out of bounds")]
                 [(not (integer? (NumV-n (cast r NumV))))
                  (error 'multOp "JILI Index out of bounds")]
                 [else (v*s (fetch (Location (cast (+ (NumV-n r) (Location-loc (ArrayV-loc l))) Integer)) sto) sto)])]
             [else (error 'getBinop "JILI Wrong Type For This Operation")])]
    [`<= (cond
           [(and (NumV? l) (NumV? r)) (v*s (BoolV (<= (NumV-n l) (NumV-n r))) sto)]
           [else (error 'getBinop "JILI Wrong Type For This Operation")])]
    [`begin (v*s r sto)]
    [else (error 'getBinop "JILI Unexpected Symbol ~e" op)]))



  ; storeValues: (Listof Value), Store -> Store
  ; Function that stores a list of Values into a Storage and return update store
(define (storeValues [vals : (Listof Value)] [sto : Store]) : Store
  (match vals
    [`() sto]
    [(cons f r) (storeValues r (storeValue f (Location (cast (length sto) Integer)) sto))]))



  ; storeValue: Value, Store -> Store
  ; Function that add value into Storage  
(define (storeValue [val : Value] [loc : Location] [sto : Store]) : Store
  (match sto
    [`() (list (Storage loc val))]
    [(cons f r) (cons (Storage loc val) sto)]))



  ; allocate: Store, Locations, Value -> Location, Store
  ; A function that allocates locations for a value in a store
(define (allocate [sto : Store] [size : Integer] [val : Value]) : l*s
  (cond
    [(equal? 0 size) (l*s (Location 0) sto)]
    [else (l*s
           (Location (length sto))
           (l*s-sto (allocate (cons (Storage (Location (length sto)) val) sto) (- size 1) val)))]))



  ; fetch: Location, Store -> Value
  ; A function that fetches the value from the store binded to the location
(define (fetch [loc : Location] [sto : Store]) : Value
  (cond
    [(empty? sto) (error 'fetch "JILI name not found: ~e ~e" loc sto)]
    [else (cond
            [(equal? loc (Storage-loc (first sto)))
             (Storage-val (first sto))]
            [else (fetch loc (rest sto))])]))



;3. Helper Functions================================================================================
  ; lookup: Symbol, Env -> Location
  ; A function that looksup the symbol in the Env and gets the binded Location
(define (lookup [for : Symbol] [env : Env]) : Location
  (cond
    [(empty? env) (error 'lookup "JILI name not found: ~e" for)]
    [else (cond
            [(equal? for (Binding-key (first env)))
             (Binding-loc (first env))]
            [else (lookup for (rest env))])]))



  ; extend-env: (Listof ExprC), (Listof Symbol), Env-> Env
  ; A function that takes an exising Env and adds new bindings created from the vals and args
(define (extend-env [size : Integer] [args : (Listof Symbol)] [curEnv : Env]) : Env
  (match args
    [`() curEnv]
    [else 
     (cons (Binding (first args) (Location size))
           (extend-env (+ size 1) (rest args) curEnv))]))



  ; serialize: Value -> String
  ; A function that takes a Value and serializes it into a String
(define (serialize [val : Value]) : String
  (match val
    [(NumV val) (~v val)]
    [(StringV val) (string-append "\"" val "\"")]
    [(BoolV val) (cond                                            
                   [val "true"]
                   [else "false"])]
    [(CloV body ids env) "#<procedure>"]
    [(OpV op) "#<primop>"]
    [(NullV null) "null"]
    [(ArrayV loc size) "array"]))



  ; checkArgs: (Listof s-expression) -> ExprC
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
         (error 'checkArgs "JILI Duplicate arg symbols or no args"))]
       [else (error 'checkArgs "JILI Arguments Have Improper Format")])]
    [(list (? symbol? symb) `= (list body ...)) (LamC (list symb) (parse body))]
    [(list (? symbol? symb) `:= body) (MutC symb (parse body))]
    [(list `var (list vars ...) `in body)
     (define symbols (getVars vars))
     (if (checkDuplicates symbols)
         (AppC (LamC symbols (parse body)) (getArgs vars))
         (error 'checkArgs "JILI Duplicates"))]
    [(list l ...)
     (AppC (parse (first l))
           (cast
            (map (lambda ([x : Sexp]) (parse x)) (cast (rest l) (Listof Sexp)))
            (Listof ExprC)))]))



  ; checkDuplicates (Listof Symbol) -> Boolean
  ; Checks the list of symbols for duplicates returning true if no duplicates exist
(define (checkDuplicates [vars : (Listof Symbol)]) : Boolean
  (cond
    [(empty? vars) true]
    [(member (first vars) (rest vars)) false]
    [(member (first vars) `(if var in =>)) false]
    [else (checkDuplicates (rest vars))]))


;4. GETTER FUNCTIONS================================================================================
 ; valsFromStars: (Listof v*s) -> (Listof Value)
 ; Function for mapping v*s's to values
(define (valsFromStars [vals : (Listof v*s)]) : (Listof Value)
  (map (lambda ([x : v*s]) (v*s-val x)) vals))



  ; getVals: (Listof ExprC), Env, Store -> (Listof v*s)
  ; Function that creates a list of v*s from exprcs
(define (getVals [vars : (Listof ExprC)] [env : Env] [sto : Store]) : (Listof v*s)
  (match vars
    [`() `()]
    [(cons f r)
     (define vStar (interp f env sto))
     (cons vStar (getVals r env (v*s-sto vStar)))]))



  ; getArgs: (Listof s-expression) -> (Listof ExprC)
  ; It parses a list of var s-expressons into a list of ExprCs by parsing each of the bodies
(define (getArgs [vars : (Listof Sexp)]) : (Listof ExprC)
  (match vars
    [`() `()]
    [(cons f r)
     (match f
       [(list id `= body) (cons (parse body) (getArgs r))]
       [other (error 'getIds "JILI id doesnt match")])]))



  ; getVars: (Listof s-expression) -> (Listof Symbol)
  ; It parses a list of var s-expressons grabbing only the id symbols and inserting them into a list
(define (getVars [vars : (Listof Sexp)]) : (Listof Symbol)
  (match vars
    [`() `()]
    [(cons f r)
     (match f
       [(list (? symbol? id) `= body) (cons id (getVars r))]
       [other (error 'getIds "JILI id doesnt match")])]))



;Testing Suite================================================================================
  ;Testing Interp
(check-equal? (interp (NumC 1) topEnv topStore) (v*s (NumV 1) topStore))
(check-equal? (interp (StringC "x") topEnv topStore) (v*s (StringV "x") topStore))
(check-equal? (interp (IdC `+) topEnv topStore) (v*s (OpV `+) topStore))
(check-equal? (interp (IdC `true) topEnv topStore) (v*s (BoolV true) topStore))
(check-equal? (interp (AppC (IdC `+) (list (NumC 1) (NumC 2))) topEnv topStore)
              (v*s (NumV 3) topStore))
(check-equal? (v*s-val (interp (AppC (LamC `() (NumC 1)) `()) topEnv topStore))
               (NumV 1))
(check-equal? (interp (LamC (list `x) (AppC (IdC `+) (list (IdC `x) (NumC 1))))
                      (list (Binding `x (Location 1)))
                      (list (Storage (Location 1) (NumV 1))))
              (v*s (CloV `(x) (AppC (IdC `+) (list (IdC `x) (NumC 1))) (list (Binding `x (Location 1))))
                   (list (Storage (Location 1) (NumV 1)))))
(check-equal? (interp (IdC `x)
                      (list (Binding `x (Location 100)))
                      (list (Storage (Location 100) (NumV 1))))
              (v*s (NumV 1) (list (Storage (Location 100) (NumV 1)))))
(check-equal? (v*s-val (interp (AppC (IdC `array) (list (NumC 3) (NumC 14) (IdC `false) (NumC 5))) topEnv topStore))
              (ArrayV (Location 16) 4))
(check-equal? (interp (MutC `+ (NumC 1)) topEnv topStore)
              (v*s (NullV null) (storeValue (NumV 1) (Location 1) topStore)))
(check-equal? (interp (AppC (LamC (list `x) (AppC (IdC `+) (list (IdC `x) (NumC 1)))) (list (NumC 1)))
                      (list (Binding `+ (Location 0))) (list (Storage (Location 0) (OpV `+))))
              (v*s (NumV 2) (list (Storage (Location 1) (NumV 1)) (Storage (Location 0) (OpV `+)))))
(check-equal? (interp (ifC (IdC `true) (IdC `true) (IdC `false)) topEnv topStore) (v*s (BoolV true) topStore))
(check-equal? (interp (ifC (IdC `false) (IdC `true) (IdC `false)) topEnv topStore) (v*s (BoolV false) topStore))
(check-equal? (interp (ifC (IdC `true) (AppC (IdC `+) (list (NumC 1) (NumC 2))) (IdC `false)) topEnv topStore)
              (v*s (NumV 3) topStore))
(check-equal? (interp (ifC (IdC `false) (IdC `false) (AppC (IdC `+) (list (NumC 1) (NumC 2)))) topEnv topStore)
              (v*s (NumV 3) topStore))
(check-exn (regexp (regexp-quote "JILI Improper ifC"))
           (lambda () (interp (ifC (IdC `+) (IdC `false) (AppC (IdC `+) (list (NumC 1) (NumC 2)))) topEnv topStore)))
(check-exn (regexp (regexp-quote "JILI Improper ifC"))
           (lambda () (interp (ifC (AppC (IdC `+) (list (NumC 4) (NumC 3))) (NumC 1) (NumC 2)) topEnv topStore)))
(check-exn (regexp (regexp-quote "JILI bad OpV"))
           (lambda () (interp (AppC (IdC `+) (list (NumC 1))) topEnv topStore)))



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
(check-equal? (top-interp (quote (begin 4))) "4")
(check-equal? (top-interp (quote ((minus => (minus 8 5)) (a b => (+ a (* -1 b)))))) "3")
(check-equal? (top-interp (quote ((seven => (seven))
                                  ((minus => (=> (minus (+ 3 10) (* 2 3))))
                                   (x y => (+ x (* -1 y))))))) "7")
(check-exn (regexp (regexp-quote "JILI Improper AppC"))
           (lambda () (top-interp '(3 4 5))))
(check-exn (regexp (regexp-quote "JILI user-error (StringC \"1234\")"))
           (lambda () (top-interp '(+ 4 (error "1234")))))
(check-exn (regexp (regexp-quote "JILI Improper AppC"))
           (lambda () (top-interp '((=> 9) 17))))
(check-exn (regexp (regexp-quote "JILI Index out of bounds"))
           (lambda () (top-interp '(var ((f = (new-array 5 false))) in (aset! f 5 19)))))
(check-exn (regexp (regexp-quote "JILI Index out of bounds"))
           (lambda () (top-interp '(var ((f = (new-array 5 false))) in (aset! f -1 19)))))
(check-exn (regexp (regexp-quote "JILI Index out of bounds"))
           (lambda () (top-interp '(var ((f = (new-array 5 false))) in (aset! f 2.3 19)))))
(check-equal? (top-interp `{var {[fact = "bogus"]}
                                in
                                {begin {fact := {n => {if {<= n 0} 1 {* n {fact {- n 1}}}}}}
                                       {fact 12}}})
              "479001600")
(check-equal? (top-interp (quote
                           (var ((f = (new-array 5 false)))
                                in
                                (begin
                                  (aset! f 0 19)
                                  (aset! f (+ 0 1) 20)
                                  (aset! f 0 87)
                                  (+ (* 100 (aref f 0)) (aref f 1))))))
              "8720")
(check-equal? (top-interp (quote (var ((a = 9) (b = (array 3 false true 19))
                                               (d = (array "otter")))
                                      in
                                      (var ((c = (=> (begin (aset! d 0 b)
                                                            (aset! b 3 333)
                                                            (+ (aref (aref d 0) 3) a)))))
                                           in
                                           (c))))) "342")


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
(check-equal? (parse '{l := 1}) (MutC 'l (NumC 1)))
(check-equal? (parse '{if true 1 false})
              (ifC (IdC `true) (NumC 1) (IdC `false)))
(check-equal? (parse '{+ 1 2})
              (AppC (IdC `+) (list (NumC 1) (NumC 2))))
(check-equal? (parse '{array 3 14 false 5})
              (AppC (IdC `array) (list (NumC 3) (NumC 14) (IdC `false) (NumC 5))))
(check-equal? (parse '{x => {+ x 2}})
              (LamC (list `x) (AppC (IdC `+) (list (IdC `x) (NumC 2)))))
(check-equal? (parse '{x y => {+ x y}})
              (LamC (list `x `y) (AppC (IdC `+) (list (IdC `x) (IdC `y)))))
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
(check-exn (regexp (regexp-quote "JILI Restricted symbol"))
           (lambda () (parse `if)))
(check-exn (regexp (regexp-quote "JILI Restricted symbol"))
           (lambda () (parse `{if 1 2})))
(check-exn (regexp (regexp-quote "JILI Duplicate arg symbols"))
           (lambda () (parse '(x x => 3))))
(check-exn (regexp (regexp-quote "JILI Arguments Have Improper Format"))
           (lambda () (parse '(3 4 5 => 6))))
(check-exn (regexp (regexp-quote "JILI Duplicates"))
           (lambda () (parse '(var ((z = (=> 3)) (z = 9)) in (z)))))
(check-exn (regexp (regexp-quote "JILI Duplicate arg symbols"))
           (lambda () (parse '{in => {+ 1 2}})))



  ;Testing Multop
(check-equal? (multOp `array (list (StringV "Apple") (NumV 0) (NumV 1)) `())
              (v*s
               (ArrayV (Location 0) 3)
               (list
                (Storage (Location 2) (NumV 1))
                (Storage (Location 1) (NumV 0))
                (Storage (Location 0) (StringV "Apple")))))
(check-equal? (multOp `aset!
                      (list (ArrayV (Location 0) 2) (NumV 1) (NumV 100))
                      (list (Storage (Location 1) (NumV 1)) (Storage (Location 0) (NumV 0))))
              (v*s
               (NullV null)
               (list
                (Storage (Location 1) (NumV 100))
                (Storage (Location 1) (NumV 1))
                (Storage (Location 0) (NumV 0)))))
(check-equal? (multOp `begin (list (StringV "Apple") (NumV 0) (NumV 1)) `())
              (v*s
               (NumV 1)
               (list
                (Storage (Location 2) (NumV 1))
                (Storage (Location 1) (NumV 0))
                (Storage (Location 0) (StringV "Apple")))))
(check-equal? (multOp `substring (list (StringV "Apple") (NumV 0) (NumV 1)) topStore)
              (v*s (StringV "A") topStore))
(check-exn (regexp (regexp-quote "JILI Wrong Type For This Operation"))
           (lambda () (multOp `aset! (list (BoolV true) (NumV 0)) topStore)))
(check-exn (regexp (regexp-quote "JILI Wrong Type For This Operation"))
           (lambda () (multOp `substring (list (BoolV true) (NumV 0) (BoolV true)) topStore)))



  ;Testing GetBinop
(check-equal? (getBinop `+ (NumV 1) (NumV 2) topStore topEnv) (v*s (NumV 3) topStore))
(check-equal? (getBinop `* (NumV 1) (NumV 2) topStore topEnv) (v*s (NumV 2) topStore))
(check-equal? (getBinop `- (NumV 1) (NumV 10) topStore topEnv) (v*s (NumV -9) topStore))
(check-equal? (getBinop `/ (NumV 1) (NumV 10) topStore topEnv) (v*s (NumV 1/10) topStore))
(check-equal? (getBinop `aref (ArrayV (Location 2) 1) (NumV 0)
                        (list (Storage (Location 1) (ArrayV (Location 2) 1)) (Storage (Location 2) (NumV 1)))
                        (list (Binding `p (Location 1))))
              (v*s (NumV 1) (list (Storage (Location 1) (ArrayV (Location 2) 1)) (Storage (Location 2) (NumV 1)))))
(check-exn (regexp (regexp-quote "JILI Wrong Type For This Operation"))
           (lambda () (getBinop `+ (BoolV true) (BoolV true) topStore topEnv)))
(check-exn (regexp (regexp-quote "JILI Wrong Type For This Operation"))
           (lambda () (getBinop `* (BoolV true) (BoolV true) topStore topEnv)))
(check-exn (regexp (regexp-quote "JILI Wrong Type For This Operation"))
           (lambda () (getBinop `- (BoolV true) (BoolV true) topStore topEnv)))
(check-exn (regexp (regexp-quote "JILI Wrong Type For This Operation"))
           (lambda () (getBinop `/ (BoolV true) (BoolV true) topStore topEnv)))
(check-exn (regexp (regexp-quote "JILI Division By Zero"))
           (lambda () (getBinop `/ (NumV 1) (NumV 0) topStore topEnv)))
(check-equal? (getBinop `equal? (NumV 2) (NumV 2) topStore topEnv) (v*s (BoolV #t) topStore))
(check-equal? (getBinop `equal? (NumV 1) (NumV 2) topStore topEnv) (v*s (BoolV #f) topStore))
(check-equal? (getBinop `equal? (BoolV #t) (BoolV #t) topStore topEnv) (v*s (BoolV #t) topStore))
(check-equal? (getBinop `equal? (BoolV #t) (BoolV #f) topStore topEnv) (v*s (BoolV #f) topStore))
(check-equal? (getBinop `equal? (StringV "a") (StringV "a") topStore topEnv) (v*s (BoolV #t) topStore))
(check-equal? (getBinop `equal? (StringV "a") (StringV "b") topStore topEnv) (v*s (BoolV #f) topStore))
(check-equal? (getBinop `equal? (NumV 1) (StringV "b") topStore topEnv) (v*s (BoolV #f) topStore))
(check-equal? (getBinop `equal? (ArrayV (Location 1) 1) (ArrayV (Location 1) 1) topStore topEnv)
              (v*s (BoolV #t) topStore))
(check-equal? (getBinop `equal? (ArrayV (Location 2) 1) (ArrayV (Location 1) 1) topStore topEnv)
              (v*s (BoolV #f) topStore))
(check-equal? (getBinop `<= (NumV 1) (NumV 1) topStore topEnv) (v*s (BoolV #t) topStore))
(check-equal? (getBinop `<= (NumV 11) (NumV 10) topStore topEnv) (v*s (BoolV #f) topStore))
(check-equal? (getBinop `<= (NumV 1) (NumV 10) topStore topEnv) (v*s (BoolV #t) topStore))
(check-exn (regexp (regexp-quote "JILI Wrong Type For This Operation"))
           (lambda () (getBinop `<= (BoolV true) (BoolV true) topStore topEnv)))
(check-exn (regexp (regexp-quote "JILI Wrong Type For This Operation"))
           (lambda () (getBinop `<= (StringV "a") (StringV "a") topStore topEnv)))
(check-exn (regexp (regexp-quote "JILI Unexpected Symbol"))
           (lambda () (getBinop `! (NumV 1) (NumV 1) topStore topEnv)))
(check-equal? (getBinop `new-array (NumV 1) (NumV 10) `() topEnv)
              (v*s (ArrayV (Location 0) 1) (list (Storage (Location 0)(NumV 10)))))
(check-equal? (getBinop `new-array (NumV 1) (StringV "wow") `() topEnv)
              (v*s (ArrayV (Location 0) 1) (list (Storage (Location 0)(StringV "wow")))))
(check-exn (regexp (regexp-quote "JILI Wrong Type For This Operation"))
           (lambda () (getBinop `new-array (BoolV false) (NumV 10) `() topEnv)))
(check-exn (regexp (regexp-quote "JILI Index out of bounds"))
           (lambda () (getBinop `new-array (NumV -1) (NumV 10) `() topEnv)))
(check-exn (regexp (regexp-quote "JILI Index out of bounds"))
           (lambda () (getBinop `new-array (NumV 0.2) (NumV 10) `() topEnv)))
(check-exn (regexp (regexp-quote "JILI Wrong Type For This Operation"))
           (lambda () (getBinop `aref (NumV -1) (NumV 10) `() topEnv)))
(check-exn (regexp (regexp-quote "JILI Index out of bounds"))
           (lambda () (getBinop `aref (ArrayV (Location 1) 1) (NumV 10) `() topEnv)))
(check-exn (regexp (regexp-quote "JILI Index out of bounds"))
           (lambda () (getBinop `aref (ArrayV (Location 1) 1) (NumV 0.3) `() topEnv)))
(check-exn (regexp (regexp-quote "JILI Index out of bounds"))
           (lambda () (getBinop `aref (ArrayV (Location 1) 1) (NumV -1) `() topEnv)))


;Testing Helper Funcs================================================================================
  ;Testing Allocate
(check-equal? (allocate topStore 1 (NumV 1)) (l*s (Location 16) (storeValue (NumV 1) (Location 16) topStore)))



  ;Testing storeValue
(check-equal? (storeValue (NumV 1) (Location 1) `()) (list (Storage (Location 1) (NumV 1))))
(check-equal? (storeValue (NumV 1) (Location 1) (list (Storage (Location 1) (NumV 1))))
              (list (Storage (Location 1) (NumV 1)) (Storage (Location 1) (NumV 1))))



  ;Testing storeValues
(check-equal? (storeValues (list (NumV 1)) `()) (list (Storage (Location 0) (NumV 1))))
(check-equal? (storeValues `() `()) `())



  ;Testing lookup
(check-equal? (lookup `true topEnv) (Location 5))
(check-equal? (lookup `x (list (Binding `x (Location 101)))) (Location 101))
(check-equal? (lookup `x (list (Binding `x (Location 102)) (Binding `y (Location 103)))) (Location 102))
(check-exn (regexp (regexp-quote "JILI name not found"))
           (lambda () (lookup `s `())))



  ;Testing fetch
(check-equal? (fetch (Location 1) topStore) (OpV `+))
(check-equal? (fetch (Location 1) topStore) (OpV `+))
(check-equal?(fetch (Location 01)(list(Storage (Location 01)(NumV 10))
                                      (Storage (Location 02)(NumV 100))))(NumV 10))
(check-equal?(fetch (Location 03)(list(Storage (Location 03)(StringV "jojo"))
                                      (Storage (Location 04)(NumV 1))))(StringV "jojo"))
(check-equal?(fetch (Location 05)(list(Storage (Location 03)(StringV "jojo"))
                                      (Storage (Location 04)(NumV 1))
                                      (Storage (Location 05)(OpV `+))))(OpV `+))
(check-equal?(fetch (Location 12)(list(Storage (Location 03)(StringV "jojo"))
                                      (Storage (Location 04)(NumV 1))
                                      (Storage (Location 12)(BoolV #t))))(BoolV #t))
(check-exn (regexp (regexp-quote "JILI name not found"))
           (lambda () (fetch (Location 16) topStore)))



  ;Testing extend-env
(check-equal? (extend-env  0 (list `x) `()) (list (Binding `x (Location 0))))
(check-equal? (extend-env  1 (list `x) (list (Binding `x (Location 85))))
              (list (Binding `x (Location 1)) (Binding `x (Location 85))))
(check-equal? (extend-env  74 (list `x `y) `())
              (list (Binding `x (Location 74)) (Binding `y (Location 75))))
(check-equal? (extend-env  44 (list `x) (list (Binding `x (Location 45))))
              (list (Binding `x (Location 44)) (Binding `x (Location 45))))
(check-equal? (extend-env 45 (list `x `y) (list (Binding `z (Location 45)))) 
              (list (Binding `x (Location 45)) (Binding `y (Location 46)) (Binding `z (Location 45))))



;Testing serialize
(check-equal? (serialize (StringV "hi")) "\"hi\"")
(check-equal? (serialize (NumV 1)) "1")
(check-equal? (serialize (StringV "yoyo")) "\"yoyo\"")
(check-equal? (serialize (StringV "how you been today?")) "\"how you been today?\"")
(check-equal? (serialize (BoolV #t)) "true")
(check-equal? (serialize (BoolV #f)) "false")
(check-equal? (serialize (CloV  (list `x)
                                (LamC '(x) (LamC '(y) (AppC (IdC '+) (list(IdC 'x) (IdC 'y)))))
                                (list (Binding `x (Location 1))))) "#<procedure>")
(check-equal? (serialize (OpV `+)) "#<primop>")
(check-equal? (serialize (NullV null)) "null")
(check-equal? (serialize (ArrayV (Location 16) 1)) "array")



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



  ;Testing valsFromStars
(check-equal? (valsFromStars (list (v*s (NumV 1) `()))) (list (NumV 1)))



  ;Testing getVals 
(check-equal? (getVals (list (NumC 1) (NumC 2) (IdC `x))
                       (list (Binding `x (Location 1)))
                       (list (Storage (Location 1) (NumV 22))))
              (list (v*s (NumV 1) (list (Storage (Location 1) (NumV 22))))
                    (v*s (NumV 2) (list (Storage (Location 1) (NumV 22))))
                    (v*s (NumV 22) (list (Storage (Location 1) (NumV 22))))))
(check-equal? (getVals `() `()`())`())



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



  ;Testing While and In-Order
#;(top-interp (quasiquote (var ((while = ,while))
                             in
                             (var ((in-order = ,in-order))
                                  in
                                  (+ (if (in-order (array 3 6 8) 3) 1 0)
                                     (if (in-order (array 6 7 3 8) 4) 0 2))))))
#;(parse (quasiquote (var ((while = ,while))
                             in
                             (var ((in-order = ,in-order))
                                  in
                                  (+ (if (in-order (array 3 6 8) 3) 1 0)
                                     (if (in-order (array 6 7 3 8) 4) 0 2))))))