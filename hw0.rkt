#lang racket
;Exercise 1.1 output the result
10
(+ 5 3 4)
(- 9 1)
(/ 6 2)
(+ (* 2 4) (- 4 6))
(define a 3)
(define b (+ a 1))
(+ a b (* a b))
(= a b)
(if (and (> b a) (< b (* a b)))
    b
    a)

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))

(+ 2 (if (> b a) b a))

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))

;Exercise 1.3 the function is big_sum
; To calculate the sum of two big numbers
; I define two function.
; big_one to calculate the biggest number
; big_two to calculate the second biggest number
; then I invoke the two funtions to calculate the sum
(define (big_sum a b c)
  (+  (big_one a b c) (big_two a b c))
  )

(define (big_one a b c)
  (if (> a b)
      (if (> a c) a c)
      (if (> b c) b c)
      ))

(define (big_two a b c)
  (if (> a b)
      (if (> a c) (if (> b c) b c) a)
      (if (> b c) (if (> a c) a c) b)
      ))

(big_sum 1 3 2)

;Exercise 1.5
; If the interpreter use the Normal-Order the procedure will return 0.
; Because in this mode the interpreter calculate the (= x 0) it is ture
; so the program execute the next statement 0.
; then the program return the 0, also the procedure (p) will infinite recursive
; but it not be invoke
; If the interpreter use the Applicative-Order the procedure can't return any numbers.
; Because in this mode the interpreter first unfold all the parameter,
; so when the procedure unfold the second parameter will infinite recursive, can't return any.
; In a word Normal-Order don't calculate all the parameter, so don't invoke (p)
; but Applicative-Order calculate all the parameter.
(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))
(test 0 (p))
