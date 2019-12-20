#lang racket
;problem 1
(define (diagonal sentences)
  (if (null? sentences)
      '()
      (output 1 (length sentences) sentences '())))

(define (length sentences)
  (if (null? sentences)
      0
      (+ 1 (length (cdr sentences)))))

(define (output path length sentences result)
  (if (> path length) 
      result 
      (output (+ path 1) length sentences
              (append result
                   (list
                     (each-result sentences (operate-list 1 path '()))
                     )))))

(define (operate-list count max op)
  (if (= max 0)
      '()
      (cond ((> count max) (append op op))
            ((= count max)
             (operate-list (+ count 1) max (append op (list car))))
            (else
             (operate-list (+ count 1) max (append op (list cdr)))))))

(define (each-result sentences ops)
  (if (null? ops)
      sentences
      (each-result ((car ops) sentences) (cdr ops))))

(define s1
  '((she loves you) (tell me why) (I want to hold my hand)))

(define s2
  '((I want) (You have a book) (He needs medicine to drink)))

(diagonal s1)
(diagonal s2)

;(define (diagonal sentences)
;  (if (null? sentences)
;      '()
;      (output 1 (length sentences) sentences '())))


;problem 2
(define (every-nth num list-of-sents)
  (if (null? list-of-sents)
      '()
      (cons (do num (car list-of-sents))
            (every-nth num (cdr list-of-sents)))))

(define (do num result)
  (if (= num 0)
      (car result)
      (do (- num 1) (cdr result))))

(every-nth 2 '((a b c d) (e f g h)))
(every-nth 1 '((a b c) (d e f) (g h i)))

;problem 3
(define (transform n fun x)
  (define (helper temp count)
    (if (= count n)
        '()
        (cons temp (helper (fun temp) (+ count 1)))))
  (helper x 0))

(define (square x)
  (* x x))

(transform 3 square 2)


;problem 4
;(a)
(define (count-occurrence elm aset)
  (if (null? aset)
      0
      (+ (count-occurrence elm (cdr aset))
         (if (equal? elm (car aset)) 1 0))))

(count-occurrence 3 '(3 5 2 3 3 6 5 4 3))


;(b)
(define (filter-list elm aset)
  (define (filter-iter lst result)
    (cond ((null? lst) result)
          ((equal? (car lst) elm) (filter-iter (cdr lst) result))
          (else (filter-iter (cdr lst) (append result (list (car lst)))))
          ))
  (filter-iter aset '()))

(filter-list 3 '(3 5 2 3 3 6 5 4 3))

