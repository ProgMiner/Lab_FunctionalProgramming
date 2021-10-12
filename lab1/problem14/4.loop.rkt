#lang racket


(define-syntax-rule (collatz-next-number n)
  (if (even? n) (/ n 2) (+ (* 3 n) 1)))

(define (collatz n)
  (cond [(>= n 1) (let ([l 1])
                    (do () ((= n 1) l)
                      (set! l (+ l 1))
                      (set! n (collatz-next-number n))))]))


(define longest (cons 0 0))

(do ([n 1 (+ n 1)]) ((> n 1000000))
  (let ([cur (collatz n)])
    (if (> cur (car longest))
      (set! longest (cons cur n)) (void))))


(display (~a "The longest Collatz sequence is started from " (cdr longest)
             " and of length " (car longest)))
(newline)
