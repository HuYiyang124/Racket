#lang racket

;problem 1
(define (cube-root x)
  (cube-iter 1 x))

(define
  (cube-iter guess x)
   (if (good-enough? guess x)
       guess
       (cube-iter (calculate guess x) x)
       )   
)
(define (good-enough? x y) (< (abs (- (* x x x) y)) 0.01))

(define (calculate y x)
  (/ (+ (/ x (* y y)) (+ y y)) 3))
;this procedure is calculate the cube of 8 and output the result is 2
(cube-root 27)

;problem 2
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))
