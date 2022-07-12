#lang racket
(require rackunit)


(define addOne (lambda (v) (+ 1 v)))

;0.1 Example
(define p01 (lambda (v) (lambda (f) v)))
(check-equal? ((p01 8) (lambda (x) 1234)) 8)


;0.2 One
(define one (lambda (f) (lambda (v) (f v))))
(check-equal? ((one (lambda (x) (+ 1 x))) 8) 9)


;1 two
(define two (lambda (f) (lambda (v) (f(f v)))))
(check-equal? ((two (lambda (x) (+ 1 x))) 8) 10)


;2 zero
(define zero (lambda (f) (lambda (arg) arg)))
(check-equal? ((zero (lambda (x) (+ 1 x))) 8) 8)


;3 add1
(define add1 (lambda (nlf)
               (lambda (f)
                 (lambda (x) ((nlf f) (f x))))))
(check-equal? (((add1 one) addOne) 1) 3)


; 2.4
(define add (lambda (nf1)
              (lambda (nf2)
                (lambda (f)
                  (lambda (arg) ((nf1 f) ((nf2 f) arg)))))))
(check-equal? ((((add one) two) addOne) 1) 4)
(check-equal? ((((add two) two) addOne) 1) 5)


; 2.5
(define tru (lambda (arg1 arg2) arg1))


; 2.6
(define fals (lambda (arg1 arg2) arg2))


; 2.7
(define if (lambda (f1 f2 f3) (f1 f2 f3)))
(check-equal? (if tru 5 4) 5)
(check-equal? (if fals 5 4) 4)
;(define if (lambda (a) (lambda (b) (lambda (c) ((a b) c)))))
;(check-equal? (((if tru) 5) 4) 5)
;(check-equal? (((if fals) 5) 4) 4)


; 2.8
(module sim-JILI5 racket
    (provide
     (rename-out [lambda fn]
                 [my-let let])
     #%module-begin
     #%datum
     #%app
     + - * / = equal? <=
     if)

 
  (define-syntax my-let
    (syntax-rules (in =)
      [(my-let [v = e] ... in eb)
       ((lambda (v ...) eb) e ...)])))

(require 'sim-JILI5)
(require rackunit)

(check-equal? {let
                  {one = {fn {function}
                             {fn {arg} {function arg}}}}
                {two = {fn {function}
                           {fn {arg} (function (function arg))}}}
                {add = {fn {nf1}
                           {fn {nf2}
                               {fn {f}
                                   {fn {arg} {{nf1 f} {{nf2 f} arg}}}}}}}
                {double = {fn {arg}
                              {* 2 arg}}}
                in ((((add one) two) double) 1)} 8)