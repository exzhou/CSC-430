#lang typed/racket
 
(require typed/rackunit)

(require racket/match)

;Simple Addition Operators
(+ 3 4)

(+ 4 5)

;Test Case Intro
(check-equal? (* 4 13) 52)

;True / False and bools
(or false true)

true

false

(check-equal? (or false true) true)

;Func add two numbers together
(define (add-nums [a : Real] [b : Real]) : Real
  (+ a b))

;Test Caser for add-nums
(check-equal? (add-nums 1 2) 3)

;Exercise 15 ==> creating a boolean func returning true if sunny is false or friday is true

(define (==> [sunny : Boolean] [friday : Boolean])
  (or (not sunny) (and friday true)))

;Testing Exercise 15

(check-equal? (==> false true) true)

(check-equal? (==> true true) true)

(check-equal? (==> false false) true)

(check-equal? (==> true false) false)

;Exercise 19 string-insert inserts a _ in the parameter string at the index specified 

(define (insert [prefix : String] [suffix : String]) : String
  (string-append (string-append prefix "_") suffix))

(define (string-insert [str : String] [i : Integer]) : String
  (if (> (string-length str) i) 
  (insert (substring str 0 i) (substring str (+ i 1) (string-length str))) str))


;Testing Excersise 19
(check-equal? (insert "er" "ic") "er_ic")

(check-equal? (string-insert "eric" 0) "_ric")

(check-equal? (string-insert "eric" 1) "e_ic")

(check-equal? (string-insert "eric" 4) "eric")

(check-equal? (string-insert "" 0) "")


;Exercise 27 Calculating the Price of Tickets

(define baseAttendance 120)
(define baseTicketPrice 5.0)
(define attendanceFactor 15)
(define discount 0.1)
(define fixed 180)
(define variableCost 0.04)


(define (attendees [ticket-price : Real]) : Real
  (- baseAttendance (* (- ticket-price baseTicketPrice) (/ attendanceFactor discount))))

(define (revenue [ticket-price : Real]) : Real
  (* ticket-price (attendees ticket-price)))

(define (cost [ticket-price : Real]) : Real
  (+ fixed (* variableCost (attendees ticket-price))))

(define (profit [ticket-price : Real]) : Real
  (- (revenue ticket-price)
     (cost ticket-price)))

;Testing Exercise 27
(check-equal? (attendees 5) 120.0)

(check-equal? (revenue 5) 600.0)

(check-equal? (cost 5) 184.8)

(check-equal? (profit 5) 415.2)

;Intervals: Creating a interval function for differing input deposits
(define (low [deposit : Real]) : Real
  (* .04 deposit))

(define (mid [deposit : Real]) : Real
  (* .045 deposit))

(define (high [deposit : Real]) : Real
  (* .05 deposit))

(define (intervals[deposit : Real]) : Real
  (if (<= deposit 1000) (low deposit)
      (if (<= deposit 5000) (mid deposit)
          (high deposit))))

;Testing Intervals
(check-= (intervals 1000) 40.0 .01)
(check-= (intervals 900) 36.0 .01)
(check-= (intervals 1001) 45.045 .01)
(check-= (intervals 5000) 225.0 .01)
(check-= (intervals 5001) 250.05 .01)
  
;Structs
;(struct desk ([width : Real] [height : Real] [depth : Real]))

;(struct bookshelves ([width : Real] [numShelves : Real] [depth : Real]))

(define-type furniture (U bookshelves desk))
(struct desk ([width : Real] [height : Real] [depth : Real]))
(struct bookshelves ([width : Real] [numShelves : Real] [depth : Real]))


(define (foot-print [f : furniture]) : Real
    (match f
      [(desk width height depth)
       (*(* width height) depth)]
      [(bookshelves width numShelves depth)
       (*(* width numShelves) depth)]))


;Testing Structs

(check-equal? (foot-print (desk 1 2 3)) 6)

(check-equal? (foot-print (desk 4 5 6)) 120)

(check-equal? (foot-print (bookshelves 4 5 6)) 120)

(check-equal? (foot-print (bookshelves 1 2 3)) 6)


