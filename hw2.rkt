#lang racket
;problem 1
;(a)
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (square n) (* n n))

(define (nmult n)
  (cond ((= n 1) 1)
      (else (* n (nmult (- n 1))))
  )
)

(nmult 1)
(nmult 5)
(nmult 7)
(nmult 8)

;(b) the function is redefined so I comment this
;(define nmult (lambda (n) (if (= n 1) 1 (* n (nmult (- n 1))))))

;(c)
;assume f(x) = n! - 3log(n)
;when x = 2 f(2) = 2 - 3log(2) < 0
; assume x = k f(k) = k! - 3log(k) <= 0
; let x = k + 1
; f(k + 1) = (k+1)! - 3log(k+1)
;          = k! * (k+1) - 3log(k+1)^(k+1) * (k+1)
;          = (k+1)(k! - 3log(k+1)^(k+1)) < (k+1)*f(k) <= 0
;so (nmult n) <= 3 log n

;problem 2
;because the b^n if n is even can convert to (b^2)^(n/2)
;so the counters of calculator is log(n)
;every time you can calculate a half so the time complexity is Θ(log n)

;problem 3
(define (fraction-needed numerator denominator n)
  (if (= n 1)
      (/ numerator denominator)
      (if (< numerator denominator)
          (fraction-needed (+ numerator 2) denominator (- n 1))
          (fraction-needed numerator (+ denominator 2) (- n 1))
          )))
(define (pi-quarter n)
  (fraction-needed 2 3 n))
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))
(define (pi n)
  (* 4.0 (product pi-quarter 1 (lambda (i) (+ i 1)) n)
  ))

(define (factorial n)
  (product (lambda (x) x)
           1
           (lambda (i) (+ i 1))
           n))

(factorial 1)
(factorial 2)
(factorial 3)
(factorial 4)
(newline)
(pi 10)
(pi 100)
(pi 1000)
(pi 10000)

;problem 4
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (combiner result (term a)))))
  (iter a null-value))
(define (sum term a next b)
  (accumulate + 0 term a next b))
(define (Product term a next b)
  (accumulate * 1 term a next b))
(define (sum-integers a b)
  (sum (lambda (x) x) a (lambda (x) (+ 1 x)) b))
(define (Factorial n)
  (product (lambda (x) x) 1 (lambda (x) (+ 1 x)) n))
(newline)
(sum-integers 1 10)
(Factorial 10)

;problem 5
(define (inc x)
  (+ x 1)
  )
(define (double f)
  (lambda (x)
    (f (f x))))
(newline)
(((double (double double)) inc) 5)
(((double (double double)) inc) 5)



