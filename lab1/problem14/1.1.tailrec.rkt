#lang racket


(define-syntax-rule (collatz-next-number n)
  (if (even? n) (/ n 2) (+ (* 3 n) 1)))

(define (collatz-length n len)
  (cond [(= n 1) len]
        [(> n 1) (collatz-length (collatz-next-number n) (+ len 1))]))

(define (collatz n)
  (collatz-length n 1))

(define (collatz-longest-n-prev n prev)
  (cond [(= n 0) prev]
        [(> n 0) (let* ([cur (cons (collatz n) n)]
                        [next (if (> (car cur) (car prev)) cur prev)])
                    (collatz-longest-n-prev (- n 1) next))]))

(define (collatz-longest-n n)
  (cond [(> n 0) (collatz-longest-n-prev n (cons 0 0))]))


(define longest (collatz-longest-n 1000000))


(display (~a "The longest Collatz sequence is started from " (cdr longest)
             " and of length " (car longest)))
(newline)
