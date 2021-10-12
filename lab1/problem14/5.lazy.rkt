#lang lazy


(define (range-from n)
  (cons n (range-from (+ n 1))))

(define-syntax-rule (collatz-next-number n)
  (if (even? n) (/ n 2) (+ (* 3 n) 1)))

(define (collatz-length n len)
  (cond [(= n 1) len]
        [(> n 1) (collatz-length (collatz-next-number n) (+ len 1))]))

(define (collatz n)
  (collatz-length n 1))


(define lns
  (let* ([ns (range-from 1)]
         [ls (map collatz ns)])
    (map cons ls ns)))


(define longest (argmax car (!! (take 1000000 lns))))


; (display (~a "The longest Collatz sequence is started from " (cdr longest)
;              " and of length " (car longest)))
; (newline)
