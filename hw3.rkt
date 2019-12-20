#lang racket

;problem 1
(define x (list (list 1 2) (list 3 4)))

(define (deep-reverse ls)
  (define (x-reverse l r)
    (cond ((null? l) r)
          ((pair? (car l)) (x-reverse (cdr l) (cons (x-reverse (car l) '()) r)))
          (else (x-reverse (cdr l) (cons (car l) r)))))
  (x-reverse ls '()))

(define (reverse ls)
  (define (x-reverse l r)
    (cond ((null? l)  r)
          (else (x-reverse (cdr l) (cons (car l) r)))
    )
  )
  (x-reverse ls '())
)

;(reverse x)
(deep-reverse x)

;problem 2
;(a)
(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))

(define (left-branch left)
  (car left))
(define (right-branch right)
  (car (cdr right)))
(define (branch-length x) (car x))
(define (branch-structure x) (car (cdr x)))

;(b)
(define (is-branch? x) (number? (branch-length x)))
(define (total-weight x)
  (if (is-branch? x)
      (branch-structure x)
      (+ (total-weight (left-branch x))
         (total-weight (right-branch x)))))

;(c)
(define (get-moment x)
  (* (branch-length x) (branch-structure x)))
(define moment-add +)
(define (total-moment x)
  (if (is-branch? x)
      (get-moment x)
      (moment-add (total-moment (left-branch x))
                  (total-moment (right-branch x)))))

(define (mobile-equilibrium? x)
  (if (is-branch? x)
      #t
      (let ((left (left-branch x))
            (right (right-branch x)))
        (if (= (total-moment left) (total-moment right))
            (and (mobile-equilibrium? left)
                 (mobile-equilibrium? right))
            #f))))

;problem 3
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* higher-terms x)))
              0
              coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))

;problem 4
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    '()
    (cons (accumulate op init (map (lambda (sub-seqs) (car sub-seqs)) seqs))
          (accumulate-n op init (map (lambda (sub-seqs) (cdr sub-seqs)) seqs)))))

;problem 5
(define (sum alist) 
(if (null? alist) 0 (+ (car alist) (sum (cdr alist)))))

(define (subsequence fun alist)
  (if (null? alist)
      '()
      (cons (fun alist) (subsequence fun (cdr alist)))))

(define test-list (list 7 9 3 5))
test-list
(subsequence sum test-list)

