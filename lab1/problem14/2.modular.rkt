#lang racket


(define (foldl1 f xs)
  (foldl f (car xs) (cdr xs)))


(define-syntax-rule (collatz-next-number n)
  (if (even? n) (/ n 2) (+ (* 3 n) 1)))

(define (collatz-length n len)
  (cond [(= n 1) len]
        [(> n 1) (collatz-length (collatz-next-number n) (+ len 1))]))

(define (collatz n)
  (collatz-length n 1))

(define (collatz-seq-n-tail n tail)
  (cond [(<= n 0) tail]
        [(> n 0) (collatz-seq-n-tail (- n 1) (cons (cons (collatz n) n) tail))]))

(define (collatz-seq-n n)
  (collatz-seq-n-tail n null))


(define lns (collatz-seq-n 1000000))

(define longest (foldl1 (lambda (elem acc) (if (> (car elem) (car acc)) elem acc)) lns))


(display (~a "The longest Collatz sequence is started from " (cdr longest)
             " and of length " (car longest)))
(newline)
