#lang typed/racket
(require typed/rackunit)

;Completed All
;Team: Eric Zhou, Yuya Shimbori, Nikhil Koganti, Harsimran Preet Singh

;1.1 Problem 2.33 A function for calculating the total profit for a theater based on the number of attendees
(define (left-spine [x : Real]) : Real
  0)
(define (occurrences [x : Real] [y : Real]) : Real
  0)
(define ticket_price 5)
(define ticket_cost .5)
(define base_cost 20)
(define (total-profit [attendees : Real]) : Real
  (- (* attendees ticket_price) (+ base_cost (* attendees ticket_cost))))

(check-equal? (total-profit 10) 25.0)
(check-equal? (total-profit 1) -15.5)
(check-equal? (total-profit 4) -2.0)
(check-equal? (total-profit 5) 2.5)
(check-equal? (total-profit 2) -11.0)
(check-equal? (total-profit 100) 430.0)
(check-equal? (left-spine 1) 0)
(check-equal? (occurrences 1 1) 0)



;1.2 Problem 3.33 Calculates the area of a cylinder given its height and radius

(define pi 3.1415926)
;(define 'pi 3.14)
   ; Helper func for finding the circumfrance of a circle
   ; circumfrance: Real -> Real
(define (circumfrance [radius : Real]) : Real
  (* radius 2 pi))
   ; Helper func for finding the area of a circle
   ; circle_area: Real -> Real
(define (circle_area [radius : Real]) : Real
  (* radius radius pi))
   ; Function for getting the circumfrance of a cylinder
   ; area-cylinder: Real, Real -> Real
(define (area-cylinder [radius : Real] [height : Real]) : Real
  (+ (circle_area radius) (circle_area radius) (* height (circumfrance radius))))
  ;(+(* 2 (circle_area radius)) (* height (circumfrance radius))))

(check-equal? (circumfrance 2) (* 4 pi))
(check-equal? (circle_area 2) (* 4 pi))
(check-equal? (circle_area 3) (* 9 pi))
(check-= (area-cylinder 2 3) (* 20 pi) .01)
(check-= (area-cylinder 3 4) (* 42 pi) .01)
(check-true (< (abs (- (area-cylinder 4 9) 326.7256359733385)) 1e-5))



;2. Card Playing Next Suite Function
   ; Type Definition for Cards
(define-type card (U Numeric FaceCard))
;(define-type Suite (list `spade `club `diamond `heart))
   ; Struct Definition for pip and kind
(struct Numeric ([suite : Symbol] [pip : Real])#:transparent)
(struct FaceCard ([suite : Symbol] [kind : Symbol])#:transparent)
   ; Function that takes in a face value name and returns the next face value name
   ; get_faceval: String -> String
(define (get_faceval [kind : Symbol]) : Symbol
  (cond
    [(equal? kind `jack) `queen]
    [(equal? kind `queen) `king]
    [else (error "Invalid Kind")]))

   ; Function that takes in a card and returns the next card in acending order with the same suite
   ; next-card: Card -> Card
(define (next-card [cur : card]) : card
  (match cur
    [(Numeric suite pip) (cond
                           [(and (real? pip) (< pip 10)) (Numeric suite (+ 1 pip))]
                           [(and (real? pip) (equal? pip 10)) (FaceCard suite `jack)]
                           [else (error "Numeric Value too Large")])]
    
    [(FaceCard suite kind) (cond
                             [(and (symbol? kind) (equal? kind `king)) (Numeric suite 1)]
                             [else (FaceCard suite (get_faceval kind))])]))

;Added in Tests for Compliance
;(check-equal? (get_faceval "king") "jack")
;(check-equal? (next-card (Numeric `heart 11)) (FaceCard `heart "jack"))
(check-exn exn:fail?
           (λ () (get_faceval `king)))
(check-exn exn:fail?
           (λ () (next-card (Numeric `heart 11))))
(check-exn exn:fail?
           (λ () (next-card (FaceCard `heart `a))))
(check-equal? (get_faceval `jack) `queen)
(check-equal? (get_faceval `queen) `king)
(check-equal? (next-card (Numeric `heart 1)) (Numeric `heart 2))
(check-equal? (next-card (Numeric `heart 10)) (FaceCard `heart `jack))
(check-equal? (next-card (FaceCard `heart `jack)) (FaceCard `heart `queen))
(check-equal? (next-card (FaceCard `heart `queen)) (FaceCard `heart `king))
(check-equal? (next-card (FaceCard `heart `king)) (Numeric `heart 1))
(check-equal? (next-card (Numeric `club 10)) (FaceCard `club `jack))



;3. Polynomials
(define-type Polynomial (U Linear Quadratic Real))
(struct Linear([A : Real] [B : Real]) #:transparent)
(struct Quadratic([A : Real] [B : Real] [C : Real]) #:transparent)
   ; Function that takes a polynomial and a real and interprets the value of the polynomial
   ; interp: Polynomial, Real-> Real
(define (interp [p : Polynomial] [x : Real]) : Real
  (match p
    [(Linear A B) (+ (* A x) B)]
    [(Quadratic A B C) (+ (* A x x) (* B x) C)]))

(check-equal? (interp (Linear 1 1) 1) 2)
(check-equal? (interp (Linear 1 1) -1) 0)
(check-equal? (interp (Quadratic 1 1 1) 1) 3)
(check-equal? (interp (Quadratic 1 1 1) -1) 1)



;4. Derivatives
   ; Function that takes a polynomial and calculates the derivative
   ; derivatives: Polynomial -> Polynomial
(define (derivative [p : Polynomial]) : Polynomial
  (match p
    [(Linear A B) (Linear 0 A)]
    [(Quadratic A B C) (cond
                         [(not (equal? A 0)) (Linear (* 2 A) B)]
                         [else (error "Cannot Use 0 as first Coefficient")])]))

(check-equal? (derivative (Linear 1 1)) (Linear 0 1))
(check-equal? (derivative (Linear 4 1)) (Linear 0 4))
(check-equal? (derivative (Quadratic 1 1 1)) (Linear 2 1))
(check-exn exn:fail?
           (λ () (derivative (Quadratic 0 1 1))))
(check-equal? (derivative (Linear -3 4)) (Linear 0 -3))



;5. Binary Tree
(define-type BTree (U Leaf Node))
(struct Leaf ([value : Symbol])#:transparent)
(struct Node ([left : BTree] [right : BTree])#:transparent)

(define l(Leaf `s))
(define n(Node l l))
#;(check-exn exn:fail?
           (λ () (Node l null)))
(define tree(Node l (Node n l)))



;6. Mirror
   ; Functiont that mirrors a Binary Tree
   ; mirror: BTree -> BTree
(define (mirror [tree : BTree]) : BTree
  (match tree
  [(Node left right) (Node (mirror right) (mirror left))]
  [(Leaf value) (Leaf value)]))

(check-equal? (mirror (Node (Leaf `left) (Leaf `right))) (Node (Leaf `right) (Leaf `left)))
(check-equal? (mirror (Node (Node (Leaf `left) (Leaf `right)) (Leaf `right)))
              (Node (Leaf `right) (Node (Leaf `right) (Leaf `left))))



;7. Min-Depth
   ; Function that calculates the shortest length from root to a leaf
   ; min-depth: BTree -> Real
(define (min-depth [tree : BTree]) : Real
  (match tree
  [(Node left right) (min (+ 1 (min-depth left)) (+ 1 (min-depth right)))]
  [(Leaf symbol) 0]))

(check-equal? (min-depth (Leaf `s)) 0)
(check-equal? (min-depth (Node (Leaf `s) (Leaf `s))) 1)



;8. Containment
   ; Function that checks if a symbol is in the BTree
   ; containment: BTree, Symbol -> Boolean
(define (contains? [tree : BTree] [target : Symbol]) : Boolean
  (match tree
    [(Node left right) (or false (or false (contains? left target)) (or false (contains? right target)))]
    [(Leaf symbol) (equal? symbol target)]))


(define containLeaf(Leaf `s))
(define containNode(Node containLeaf containLeaf))
(define notContainLeaf(Leaf `t))
(define notContainNode(Node notContainLeaf notContainLeaf))
(check-true (contains? containLeaf `s))
(check-true (contains? containNode `s))
(check-false (contains? notContainLeaf `s))
(check-false (contains? notContainNode `s))



;9. Substitution
   ; Helper function that replaces any instance of target symbol with replaceTree within sourceTree
   ; replace: BTree, Symbol, BTree -> BTree
(define (replace [sourceTree : BTree] [target : Symbol] [replaceTree : BTree]) : BTree
  (match sourceTree
    [(Leaf symbol) (cond
                     [(equal? symbol target) replaceTree]
                     [else sourceTree])]
    [(Node left right) (Node (subst left target replaceTree) (subst right target replaceTree))]))
   ; Function that substitutes any instance of symbol with replaceTree within sourceTree
   ; subst: BTree, Symbol, BTree -> BTree
(define (subst [sourceTree : BTree] [target : Symbol] [replaceTree : BTree]) : BTree
  (cond
    [(contains? sourceTree target) (replace sourceTree target replaceTree)]
    [else sourceTree]))

(define target `target)
(define subLeaf(Leaf `target))
(define subNode(Node subLeaf subLeaf))
(define subTree(Node subNode subNode))
(define replaceLeaf(Leaf `sub))
(define replaceNode(Node replaceLeaf replaceLeaf))
(define replaceTree(Node replaceNode replaceNode))
(define returnNode(Node replaceTree replaceTree))
(define returnTree(Node returnNode returnNode))

(check-equal? (contains? subLeaf `target) true)
(check-equal? (replace replaceLeaf `target replaceLeaf) replaceLeaf)
(check-equal? (subst subLeaf `target replaceLeaf) replaceLeaf)
(check-equal? (subst subLeaf `target replaceTree) replaceTree)
(check-equal? (subst subTree `target replaceTree) returnTree)
(check-equal? (subst replaceTree `target replaceTree) replaceTree)