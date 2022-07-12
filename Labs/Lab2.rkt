#lang typed/racket
(require typed/rackunit)

;1. rev-str-app: Listof String -> String
   ; Function that combines a list of strings into a reversed conjoined string
(define (rev-str-app [s : (Listof String)]) : String
  (match s
    [`() ""]
    [(cons s r) (string-append (rev-str-app r) s)]))

#;(define (rev-str-app [s : (Listof String)]) : String
  (match s
    [`() ...]
    [(cons s r) ... (rev-str-app r) s]))

(check-equal? (rev-str-app `("a" "b" "c")) "cba")
(check-equal? (rev-str-app `("ball" "juice" "frog")) "frogjuiceball")
(check-equal? (rev-str-app `()) "")


;2. Print Type

;Printing the type of rev-str-app:
;The result of the printing of type rev-str-app was Listof String and String. This makes sense as Listof String is the input type and String is the return type of the function.
;Print Type seems to print the input types of the function and the output types of the function as the function type. 

;Printint the Type of + :
;Addition seems to be very long since it needs to account for all the different valid input types and the resultant types that can be performed with +.
#;(case->
 (-> Zero)
 (-> Number Number)
 (-> Zero Zero Zero)
 (-> Number Zero Number)
 (-> Zero Number Number)
 (-> Positive-Byte Positive-Byte Positive-Index)
 (-> Byte Byte Index)
 (-> Positive-Byte Positive-Byte Positive-Byte Positive-Index)
 (-> Byte Byte Byte Index)
 (-> Positive-Index Index Positive-Fixnum)
 (-> Index Positive-Index Positive-Fixnum)
 (-> Positive-Index Index Index Positive-Fixnum)
 (-> Index Positive-Index Index Positive-Fixnum)
 (-> Index Index Positive-Index Positive-Fixnum)
 (->* (Index Index) (Index) Nonnegative-Fixnum)
 (-> Negative-Fixnum One Nonpositive-Fixnum)
 (-> One Negative-Fixnum Nonpositive-Fixnum)
 (-> Nonpositive-Fixnum Nonnegative-Fixnum Fixnum)
 (-> Nonnegative-Fixnum Nonpositive-Fixnum Fixnum)
 (-> Positive-Integer
     Nonnegative-Integer
     Nonnegative-Integer
     *
     Positive-Integer)
 (-> Nonnegative-Integer
     Positive-Integer
     Nonnegative-Integer
     *
     Positive-Integer)
 (-> Nonnegative-Integer
     Nonnegative-Integer
     Positive-Integer
     Nonnegative-Integer
     *
     Positive-Integer)
 (-> Negative-Integer
     Nonpositive-Integer
     Nonpositive-Integer
     *
     Negative-Integer)
 (-> Nonpositive-Integer
     Negative-Integer
     Nonpositive-Integer
     *
     Negative-Integer)
 (-> Nonpositive-Integer
     Nonpositive-Integer
     Negative-Integer
     Nonpositive-Integer
     *
     Negative-Integer)
 (-> Nonnegative-Integer * Nonnegative-Integer)
 (-> Nonpositive-Integer * Nonpositive-Integer)
 (-> Integer * Integer)
 (-> Positive-Exact-Rational
     Nonnegative-Exact-Rational
     Nonnegative-Exact-Rational
     *
     Positive-Exact-Rational)
 (-> Nonnegative-Exact-Rational
     Positive-Exact-Rational
     Nonnegative-Exact-Rational
     *
     Positive-Exact-Rational)
 (-> Nonnegative-Exact-Rational
     Nonnegative-Exact-Rational
     Positive-Exact-Rational
     Nonnegative-Exact-Rational
     *
     Positive-Exact-Rational)
 (-> Negative-Exact-Rational
     Nonpositive-Exact-Rational
     Nonpositive-Exact-Rational
     *
     Negative-Exact-Rational)
 (-> Nonpositive-Exact-Rational
     Negative-Exact-Rational
     Nonpositive-Exact-Rational
     *
     Negative-Exact-Rational)
 (-> Nonpositive-Exact-Rational
     Nonpositive-Exact-Rational
     Negative-Exact-Rational
     Nonpositive-Exact-Rational
     *
     Negative-Exact-Rational)
 (-> Nonnegative-Exact-Rational * Nonnegative-Exact-Rational)
 (-> Nonpositive-Exact-Rational * Nonpositive-Exact-Rational)
 (-> Exact-Rational * Exact-Rational)
 (-> Positive-Flonum Nonnegative-Real Nonnegative-Real * Positive-Flonum)
 (-> Nonnegative-Real Positive-Flonum Nonnegative-Real * Positive-Flonum)
 (-> Nonnegative-Real
     Nonnegative-Real
     Positive-Flonum
     Nonnegative-Real
     *
     Positive-Flonum)
 (-> Positive-Real Nonnegative-Flonum Nonnegative-Flonum * Positive-Flonum)
 (-> Nonnegative-Flonum Positive-Real Nonnegative-Flonum * Positive-Flonum)
 (-> Nonnegative-Flonum
     Nonnegative-Flonum
     Positive-Real
     Nonnegative-Flonum
     *
     Positive-Flonum)
 (-> Negative-Flonum Nonpositive-Real Nonpositive-Real * Negative-Flonum)
 (-> Nonpositive-Real Negative-Flonum Nonpositive-Real * Negative-Flonum)
 (-> Nonpositive-Real
     Nonpositive-Real
     Negative-Flonum
     Nonpositive-Real
     *
     Negative-Flonum)
 (-> Negative-Real Nonpositive-Flonum Nonpositive-Flonum * Negative-Flonum)
 (-> Nonpositive-Flonum Negative-Real Nonpositive-Flonum * Negative-Flonum)
 (-> Nonpositive-Flonum
     Nonpositive-Flonum
     Negative-Real
     Nonpositive-Flonum
     *
     Negative-Flonum)
 (-> Nonnegative-Flonum Nonnegative-Real Nonnegative-Real * Nonnegative-Flonum)
 (-> Nonnegative-Real Nonnegative-Flonum Nonnegative-Real * Nonnegative-Flonum)
 (-> Nonnegative-Real
     Nonnegative-Real
     Nonnegative-Flonum
     Nonnegative-Real
     *
     Nonnegative-Flonum)
 (-> Nonpositive-Flonum Nonpositive-Real Nonpositive-Real * Nonpositive-Flonum)
 (-> Nonpositive-Real Nonpositive-Flonum Nonpositive-Real * Nonpositive-Flonum)
 (-> Nonpositive-Real
     Nonpositive-Real
     Nonpositive-Flonum
     Nonpositive-Real
     *
     Nonpositive-Flonum)
 (-> Flonum Real Real * Flonum)
 (-> Real Flonum Real * Flonum)
 (-> Real Real Flonum Real * Flonum)
 (-> Flonum Flonum * Flonum)
 (-> Positive-Single-Flonum
     (U Nonnegative-Exact-Rational Nonnegative-Single-Flonum)
     (U Nonnegative-Exact-Rational Nonnegative-Single-Flonum)
     *
     Positive-Single-Flonum)
 (-> (U Nonnegative-Exact-Rational Nonnegative-Single-Flonum)
     Positive-Single-Flonum
     (U Nonnegative-Exact-Rational Nonnegative-Single-Flonum)
     *
     Positive-Single-Flonum)
 (-> (U Nonnegative-Exact-Rational Nonnegative-Single-Flonum)
     (U Nonnegative-Exact-Rational Nonnegative-Single-Flonum)
     Positive-Single-Flonum
     (U Nonnegative-Exact-Rational Nonnegative-Single-Flonum)
     *
     Positive-Single-Flonum)
 (-> (U Positive-Exact-Rational Positive-Single-Flonum)
     Nonnegative-Single-Flonum
     Nonnegative-Single-Flonum
     *
     Positive-Single-Flonum)
 (-> Nonnegative-Single-Flonum
     (U Positive-Exact-Rational Positive-Single-Flonum)
     Nonnegative-Single-Flonum
     *
     Positive-Single-Flonum)
 (-> Nonnegative-Single-Flonum
     Nonnegative-Single-Flonum
     (U Positive-Exact-Rational Positive-Single-Flonum)
     Nonnegative-Single-Flonum
     *
     Positive-Single-Flonum)
 (-> Negative-Single-Flonum
     (U Nonpositive-Exact-Rational Nonpositive-Single-Flonum)
     (U Nonpositive-Exact-Rational Nonpositive-Single-Flonum)
     *
     Negative-Single-Flonum)
 (-> (U Nonpositive-Exact-Rational Nonpositive-Single-Flonum)
     Negative-Single-Flonum
     (U Nonpositive-Exact-Rational Nonpositive-Single-Flonum)
     *
     Negative-Single-Flonum)
 (-> (U Nonpositive-Exact-Rational Nonpositive-Single-Flonum)
     (U Nonpositive-Exact-Rational Nonpositive-Single-Flonum)
     Negative-Single-Flonum
     (U Nonpositive-Exact-Rational Nonpositive-Single-Flonum)
     *
     Negative-Single-Flonum)
 (-> (U Negative-Exact-Rational Negative-Single-Flonum)
     Nonpositive-Single-Flonum
     Nonpositive-Single-Flonum
     *
     Negative-Single-Flonum)
 (-> Nonpositive-Single-Flonum
     (U Negative-Exact-Rational Negative-Single-Flonum)
     Nonpositive-Single-Flonum
     *
     Negative-Single-Flonum)
 (-> Nonpositive-Single-Flonum
     Nonpositive-Single-Flonum
     (U Negative-Exact-Rational Negative-Single-Flonum)
     Nonpositive-Single-Flonum
     *
     Negative-Single-Flonum)
 (-> Nonnegative-Single-Flonum
     (U Nonnegative-Exact-Rational Nonnegative-Single-Flonum)
     (U Nonnegative-Exact-Rational Nonnegative-Single-Flonum)
     *
     Nonnegative-Single-Flonum)
 (-> (U Nonnegative-Exact-Rational Nonnegative-Single-Flonum)
     Nonnegative-Single-Flonum
     (U Nonnegative-Exact-Rational Nonnegative-Single-Flonum)
     *
     Nonnegative-Single-Flonum)
 (-> (U Nonnegative-Exact-Rational Nonnegative-Single-Flonum)
     (U Nonnegative-Exact-Rational Nonnegative-Single-Flonum)
     Nonnegative-Single-Flonum
     (U Nonnegative-Exact-Rational Nonnegative-Single-Flonum)
     *
     Nonnegative-Single-Flonum)
 (-> Nonpositive-Single-Flonum
     (U Nonpositive-Exact-Rational Nonpositive-Single-Flonum)
     (U Nonpositive-Exact-Rational Nonpositive-Single-Flonum)
     *
     Nonpositive-Single-Flonum)
 (-> (U Nonpositive-Exact-Rational Nonpositive-Single-Flonum)
     Nonpositive-Single-Flonum
     (U Nonpositive-Exact-Rational Nonpositive-Single-Flonum)
     *
     Nonpositive-Single-Flonum)
 (-> (U Nonpositive-Exact-Rational Nonpositive-Single-Flonum)
     (U Nonpositive-Exact-Rational Nonpositive-Single-Flonum)
     Nonpositive-Single-Flonum
     (U Nonpositive-Exact-Rational Nonpositive-Single-Flonum)
     *
     Nonpositive-Single-Flonum)
 (-> Single-Flonum
     (U Exact-Rational Single-Flonum)
     (U Exact-Rational Single-Flonum)
     *
     Single-Flonum)
 (-> (U Exact-Rational Single-Flonum)
     Single-Flonum
     (U Exact-Rational Single-Flonum)
     *
     Single-Flonum)
 (-> (U Exact-Rational Single-Flonum)
     (U Exact-Rational Single-Flonum)
     Single-Flonum
     (U Exact-Rational Single-Flonum)
     *
     Single-Flonum)
 (-> Single-Flonum Single-Flonum * Single-Flonum)
 (-> Positive-Inexact-Real
     Nonnegative-Real
     Nonnegative-Real
     *
     Positive-Inexact-Real)
 (-> Nonnegative-Real
     Positive-Inexact-Real
     Nonnegative-Real
     *
     Positive-Inexact-Real)
 (-> Nonnegative-Real
     Nonnegative-Real
     Positive-Inexact-Real
     Nonnegative-Real
     *
     Positive-Inexact-Real)
 (-> Positive-Real
     Nonnegative-Inexact-Real
     Nonnegative-Inexact-Real
     *
     Positive-Inexact-Real)
 (-> Nonnegative-Inexact-Real
     Positive-Real
     Nonnegative-Inexact-Real
     *
     Positive-Inexact-Real)
 (-> Nonnegative-Inexact-Real
     Nonnegative-Inexact-Real
     Positive-Real
     Nonnegative-Inexact-Real
     *
     Positive-Inexact-Real)
 (-> Negative-Inexact-Real
     Nonpositive-Real
     Nonpositive-Real
     *
     Negative-Inexact-Real)
 (-> Nonpositive-Real
     Negative-Inexact-Real
     Nonpositive-Real
     *
     Negative-Inexact-Real)
 (-> Nonpositive-Real
     Nonpositive-Real
     Negative-Inexact-Real
     Nonpositive-Real
     *
     Negative-Inexact-Real)
 (-> Negative-Real
     Nonpositive-Inexact-Real
     Nonpositive-Inexact-Real
     *
     Negative-Inexact-Real)
 (-> Nonpositive-Inexact-Real
     Negative-Real
     Nonpositive-Inexact-Real
     *
     Negative-Inexact-Real)
 (-> Nonpositive-Inexact-Real
     Nonpositive-Inexact-Real
     Negative-Real
     Nonpositive-Inexact-Real
     *
     Negative-Inexact-Real)
 (-> Nonnegative-Inexact-Real
     Nonnegative-Real
     Nonnegative-Real
     *
     Nonnegative-Inexact-Real)
 (-> Nonnegative-Real
     Nonnegative-Inexact-Real
     Nonnegative-Real
     *
     Nonnegative-Inexact-Real)
 (-> Nonnegative-Real
     Nonnegative-Real
     Nonnegative-Inexact-Real
     Nonnegative-Real
     *
     Nonnegative-Inexact-Real)
 (-> Nonpositive-Inexact-Real
     Nonpositive-Real
     Nonpositive-Real
     *
     Nonpositive-Inexact-Real)
 (-> Nonpositive-Real
     Nonpositive-Inexact-Real
     Nonpositive-Real
     *
     Nonpositive-Inexact-Real)
 (-> Nonpositive-Real
     Nonpositive-Real
     Nonpositive-Inexact-Real
     Nonpositive-Real
     *
     Nonpositive-Inexact-Real)
 (-> Inexact-Real Real Real * Inexact-Real)
 (-> Real Inexact-Real Real * Inexact-Real)
 (-> Real Real Inexact-Real Real * Inexact-Real)
 (-> Positive-Real Nonnegative-Real Nonnegative-Real * Positive-Real)
 (-> Nonnegative-Real Positive-Real Nonnegative-Real * Positive-Real)
 (-> Nonnegative-Real
     Nonnegative-Real
     Positive-Real
     Nonnegative-Real
     *
     Positive-Real)
 (-> Negative-Real Nonpositive-Real Nonpositive-Real * Negative-Real)
 (-> Nonpositive-Real Negative-Real Nonpositive-Real * Negative-Real)
 (-> Nonpositive-Real
     Nonpositive-Real
     Negative-Real
     Nonpositive-Real
     *
     Negative-Real)
 (-> Nonnegative-Real * Nonnegative-Real)
 (-> Nonpositive-Real * Nonpositive-Real)
 (-> Real * Real)
 (-> Exact-Number * Exact-Number)
 (-> Float-Complex Number Number * Float-Complex)
 (-> Number Float-Complex Number * Float-Complex)
 (-> Number Number Float-Complex Number * Float-Complex)
 (-> Flonum Inexact-Complex Inexact-Complex * Float-Complex)
 (-> Inexact-Complex Flonum Inexact-Complex * Float-Complex)
 (-> Inexact-Complex Inexact-Complex Flonum Inexact-Complex * Float-Complex)
 (-> Single-Flonum-Complex
     (U Exact-Rational Single-Flonum Single-Flonum-Complex)
     (U Exact-Rational Single-Flonum Single-Flonum-Complex)
     *
     Single-Flonum-Complex)
 (-> (U Exact-Rational Single-Flonum Single-Flonum-Complex)
     Single-Flonum-Complex
     (U Exact-Rational Single-Flonum Single-Flonum-Complex)
     *
     Single-Flonum-Complex)
 (-> (U Exact-Rational Single-Flonum Single-Flonum-Complex)
     (U Exact-Rational Single-Flonum Single-Flonum-Complex)
     Single-Flonum-Complex
     (U Exact-Rational Single-Flonum Single-Flonum-Complex)
     *
     Single-Flonum-Complex)
 (-> Inexact-Complex
     (U Inexact-Complex Real)
     (U Inexact-Complex Real)
     *
     Inexact-Complex)
 (-> (U Inexact-Complex Real)
     Inexact-Complex
     (U Inexact-Complex Real)
     *
     Inexact-Complex)
 (-> (U Inexact-Complex Real)
     (U Inexact-Complex Real)
     Inexact-Complex
     (U Inexact-Complex Real)
     *
     Inexact-Complex)
 (-> Number * Number))


;3. Bicycle
   ; Type Definiton for Bicycle

(define-type Bicycle (U Trek Bianchi Gunnar))
(struct Trek ([value : Real])#:transparent)
(struct Bianchi ([value : Real])#:transparent)
(struct Gunnar ([value : Real])#:transparent)



;4. Only-Treks
   ; Function that takes a list of Bicycles and returns a list of only Treks
   ; only-treks: Listof Bicycle -> Listof Trek
(define (only-treks [l : (Listof Bicycle)]) : (Listof Trek)
  (match l
    [`() `()]
    [(cons b r) (cond
                  [(Trek? b) (cons b (only-treks r))]
                  [else (only-treks r)])]))

(check-equal? (only-treks (list (Trek 1) (Trek 2) (Gunnar 3))) (list (Trek 1) (Trek 2)))
(check-equal? (only-treks (list (Bianchi 1) (Bianchi 2) (Gunnar 3))) `())



;5. Only-Bianchis
   ; Function that takes a list of Bicycles and returns a list of only Bianchis
   ; only-bianchis: Listof Bicycle -> Listof Bianchis
(define (only-bianchis [l : (Listof Bicycle)]) : (Listof Bianchi)
  (match l
    [`() `()]
    [(cons b r) (cond
                  [(Bianchi? b) (cons b (only-bianchis r))]
                  [else (only-bianchis r)])]))

(check-equal? (only-bianchis (list (Bianchi 1) (Bianchi 2) (Gunnar 3))) (list (Bianchi 1) (Bianchi 2)))
(check-equal? (only-bianchis (list (Trek 1) (Trek 2) (Gunnar 3))) `())



;6. Only-These
   ; Function that takes a list of Bicycles and a predicate and returns a list of only the type specified by the predicate
   ; only-these: Listof Bicycle -> Listof Bicycle
(define (only-these [l : (Listof Bicycle)] [predicate : (-> Bicycle Boolean)]) : (Listof Bicycle)
  (match l
    [`() `()]
    [(cons b r) (cond
                  [(predicate b) (cons b (only-these r predicate))]
                  [else (only-these r predicate)])]))

(check-equal? (only-these (list (Trek 1) (Bianchi 2) (Gunnar 3)) Trek?) (list (Trek 1)))
(check-equal? (only-these (list (Trek 1) (Bianchi 2) (Gunnar 3)) Bianchi?) (list (Bianchi 2)))
(check-equal? (only-these (list (Trek 1) (Bianchi 2) (Gunnar 3)) Gunnar?) (list (Gunnar 3)))



;7. My Append
   ; Function that appends two lists of strings into a single list
   ; my-append: Listof String, Listof String -> Listof String
(define (my-append [l1 : (Listof String)] [l2 : (Listof String)]) : (Listof String)
  (cond
    [(empty? l1)  l2]
    [else (cons (first l1) (my-append (rest l1) l2))]))

(check-equal? (my-append `("a" "b" "c") `("d" "e" "f")) `("a" "b" "c" "d" "e" "f"))
(check-equal? (my-append `() `("d" "e" "f")) `("d" "e" "f"))
(check-equal? (my-append `("a" "b" "c") `()) `("a" "b" "c"))
(check-equal? (my-append `() `()) `())

;8. My Take
   ; Function that reduces a list of any to only have n values
   ; my-append: Listof Any, Real -> Listof Any
(define (my-take [l : (Listof Any)] [n : Real]) : (Listof Any)
  (cond
    [(empty? l) `()]
    ;[(>= n (length l)) l]
    [(> n 0) (cons (first l) (my-take (rest l) (- n 1)))]
    [else `()]))

(check-equal? (my-take `() 4) `())
(check-equal? (my-take `(a) 0) `())
(check-equal? (my-take `(a b c d) 3) `(a b c))
(check-equal? (my-take `(a b c d) 4) `(a b c d))
(check-equal? (my-take `(a b c d) 5) `(a b c d))
(check-equal? (my-take `(a b c d 1 2 3 4 "apple" "orange" "bannana") 9) `(a b c d 1 2 3 4 "apple"))
;(check-true ((Real? `(12 13))))
