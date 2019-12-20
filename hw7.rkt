#lang racket
;;===================================
; CS220 Fall 2019, CS220_hw7-code.rkt
;;===================================
;
(define nil '())

(define (write-line term)
  (begin (display term)
         (newline)))

;;; some basic stream operations

(define (stream-map proc stream)
  (if (stream-empty? stream)
      empty-stream
      (stream-cons (proc (stream-first stream))
                   (stream-map proc (stream-rest stream)))))

(define (add-streams s1 s2)
  (cond ((stream-empty? s1) s2)
        ((stream-empty? s2) s1)
        (else
         (stream-cons (+ (stream-first s1) (stream-first s2))
                      (add-streams (stream-rest s1)
                                   (stream-rest s2)))))) 

(define (scale-stream c stream)
  (stream-map (lambda (x) (* x c)) stream))

;;; power series operations

(define add-series add-streams)

(define scale-series scale-stream)

(define (negate-series s)
  (scale-series -1 s))

(define (subtract-series s1 s2)
  (add-series s1 (negate-series s2)))

;;; display the first n coefficients of a series

(define (show-series s nterms)
  (if (= nterms 0)
      'done
      (begin (write-line (stream-first s))
             (show-series (stream-rest s) (- nterms 1)))))

;;; return the coefficient of x^n

(define (series-coeff s n)  (stream-ref s n))

;;; create a (finite) series from a list of coefficients

(define (coeffs->series list-of-coeffs)
  (define zeros (stream-cons 0 zeros))
  (define (iter lst)
    (if (null? lst)
        zeros
        (stream-cons (car lst)
                     (iter (cdr lst)))))
  (iter list-of-coeffs))

;;; create a series from a procedure: nth term is P(n)
;;; requires non-neg-integers to be 0,1,2,3....
(define ones (stream-cons 1 ones))

;
; Problem 1 - must be copied to as a comment
; define non-neg-integers here
; needed for Problem 1 solution
(define non-neg-integers
  (stream-cons 0 (stream-map inc non-neg-integers))) 
(define (proc->series proc)
  (stream-map proc non-neg-integers))

(display "Exercise 1")
(newline)
; Exercise 1 : the definitions are copied from the class note.
(define (stream-enumerate-interval low high)
  (if (> low high)
      empty-stream
      (stream-cons
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-first s)
      (stream-ref (stream-rest s) (- n 1))))

(define (display-line x)
  (newline)
  (display x))

(define (show x)
  (display-line x)
  x)

(define x (stream-map show (stream-enumerate-interval 0 10)))

(stream-ref x 7)

;77
; You need to explain why the output looks as you can see.
; Explain
; "Your answer should be here."
;Answer: the first 7 is in "show", the (display-line 7)
;        the second 7 is in "stream-ref", the return value of (show 7)
;        it is also (stream-enumerate-interval 0 10) is return the stream 0 to 10
;        (stream-ref x 7) is return the 8th value and it is 7, map the value to the (show x)
;        so (show x) display a 7 and return a 7 to strean-ref

;===========================================================================

(display "Exercise 2")
(newline)
; Exercise 2
(define integers (stream-cons 1 (add-streams integers ones)))
(define A (stream-cons 1 (scale-stream 2 A)))

(define (mul-streams a b)
  (stream-cons (* (stream-first a) (stream-first b))
               (mul-streams (stream-rest a)
                            (stream-rest b))))

(define B (stream-cons 1 (mul-streams B integers)))

; What is A?
;Answer: A is 2^n.
; What is B?
;Answer: B is n!.

;===========================================================================

(display "Exercise 3")
(newline)
;Exercise 3
(define (stream-pairs s)
  (if (stream-empty? s)
       empty-stream
       (stream-append
         (stream-map
          (lambda (sn) (list (stream-first s) sn))
          (stream-rest s))
         (stream-pairs (stream-rest s)))))
(show-series(stream-pairs (stream 1 2 3 4 5)) 10)
; Answers must come here!!
;(a)
;Answer:Output a <#stream>
;       ((1 2) (1 3) (1 4) (1 5)
;              (2 3) (2 4) (2 5)
;                    (3 4) (3 5)
;                          (4 5))
;(b)
;Answer: Generate all the possible pairs combination of a stream.
;(c)
;Answer: (1 2) (1 3) (1 4)...
;        output the finit value, like (show-series(stream-pairs (stream 1 2 3 4 5)) 10)

;===========================================================================

(display "Problem 1")
(newline)
;Problem 1
;The following line is copied from above!!!
;(define non-neg-integers "you need to write program text")
; test your definition.
;(show-series non-neg-integers 10)

;(define S1 "Your Progrm")
;(show-series S1 10)

(define (inc x) (+ x 1))

(stream-ref non-neg-integers 10)
(define (partial-sums s)
  (stream-cons (stream-first s)
               (add-streams (partial-sums s)
                            (stream-rest s))))
(define x-powers (lambda (x)
                   (stream-cons 1 (scale-stream x (x-powers x)))))

;the coefficient is all one
(display "S1:")
(define S1 ones)
(show-series S1 8)
;(define S2 "Your Progrm")
(display "S2:")
(define S2 integers)
(show-series S2 8)

;===========================================================================

(display "Problem 2")
(newline)
;Problem 2
(define (mul-series s1 s2)
  (stream-cons (* (stream-first s1)
                  (stream-first s2))
               (add-series (scale-series (stream-first s1) (stream-rest s2))
                           (mul-series (stream-rest s1) s2))))

;Here we check if s1*s1=s2:
(define s2 (mul-series S1 S1))
s2
;Value: "s3 --> (1 . #[promise 27])"
;(show-series (subtract-series S3 S2) 6)
;What is the coefficient of x^10 in s2*s2? 
;(series-coeff (mul-series S2 S2) 10)
;Value:  286
(display "series coeff of s2*s2")
(newline)
(series-coeff (mul-series s2 s2) 10)

;===========================================================================

(display "Problem 3")
(newline)
;Problem 3
;(define (invert-unit-series s)
(define (invert-unit-series s)
  (define X (stream-cons
             1
             (scale-series
              -1
              (mul-series (stream-rest s)
                          X))))
  X)
(define inverse-S1 (invert-unit-series S1))
(display "inverse of S1:")
(newline)
(show-series inverse-S1 10)
(display "S1 * inverse-S1:")
(newline)
(show-series (mul-series inverse-S1 S1) 10)

;===========================================================================

(display "Problem 4")
(newline)
;Problem 4
(define (div-series s1 s2)
  (let ((c (stream-first s2)))
   (if (= c 0)
       (error "constant term of s2 can't be 0!")
        (scale-series (/ 1 c)
                     (mul-series s1
                                  (invert-unit-series
                                    (scale-series (/ 1 c) s2)))))))
(display "S1/S2:")
(newline)
(show-series (div-series S1 S2) 10)

;===========================================================================

(display "Problem 5")
(newline)
;Problem 5
(define (series-map proc . argstreams)
  (if (stream-empty? (car argstreams))
      '()
      (stream-cons
       (apply proc (map (lambda (s) (stream-first s)) argstreams))
       (apply series-map
              (cons proc (map (lambda (s) (stream-rest s)) argstreams)
                    )))))
(define (integrate-series-tail s)
  (series-map * (series-map / ones integers) s))
(display "Integrate series of S2:")
(newline)
(show-series (integrate-series-tail S2) 10)

;===========================================================================

(display "Problem 6")
(newline)
;Problem 6
;Answer: e power x its' derivative is itself
(define sine-series (stream-cons 0
                        (integrate-series-tail cosine-series)))
(define cosine-series (stream-cons 1 (integrate-series-tail
                          (scale-series -1 sine-series))))
(display "Sine:")
(show-series sine-series 10)
(display "Cosine:")
(show-series cosine-series 10)

