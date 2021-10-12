#lang racket


(define-syntax-rule (collatz-next-number n)
  (if (even? n) (/ n 2) (+ (* 3 n) 1)))

(define (collatz-length n len)
  (cond [(= n 1) len]
        [(> n 1) (collatz-length (collatz-next-number n) (+ len 1))]))

(define (collatz n)
  (collatz-length n 1))


(define lns
  (let* ([ns (range 1 1000001)]
         [ls (map collatz ns)])
    (map cons ls ns)))

(define longest (argmax car lns))


(display (~a "The longest Collatz sequence is started from " (cdr longest)
             " and of length " (car longest)))
(newline)
