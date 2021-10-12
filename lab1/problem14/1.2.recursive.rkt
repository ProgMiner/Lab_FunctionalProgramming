#lang racket


(define-syntax-rule (collatz-next-number n)
  (if (even? n) (/ n 2) (+ (* 3 n) 1)))

(define (collatz n)
  (cond [(= n 1) 1]
        [(> n 1) (+ (collatz (collatz-next-number n)) 1)]))

(define (collatz-longest-n n)
  (cond [(= n 0) (cons 0 0)]
        [(> n 0) (let ([cur (collatz n)]
                       [prev (collatz-longest-n (- n 1))])
                   (if (> cur (car prev)) (cons cur n) prev))]))


(define longest (collatz-longest-n 1000000))


(display (~a "The longest Collatz sequence is started from " (cdr longest)
             " and of length " (car longest)))
(newline)
