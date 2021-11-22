#lang racket

(require "llist.rkt")


(define fib
  (llist-map
    (lambda (n)
      (if (= n 0) 1
        (if (= n 1) 1
          (+
            (llist-ref fib (- n 1))
            (llist-ref fib (- n 2))))))
    (llist-range)))

(define (main)
  (display "> ")
  (display (llist-ref fib (string->number (read-line (current-input-port) 'any))))
  (newline)
  (main))


(main)
