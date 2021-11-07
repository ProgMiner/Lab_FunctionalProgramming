#lang racket


(define (foldl1 f xs)
  (foldl f (car xs) (cdr xs)))


(define (count-number-letters n)
  (cond [(= n 1) 3]
        [(= n 2) 3]
        [(= n 3) 5]
        [(= n 4) 4]
        [(= n 5) 4]
        [(= n 6) 3]
        [(= n 7) 5]
        [(= n 8) 5]
        [(= n 9) 4]
        [(= n 10) 3]
        [(= n 11) 6]
        [(= n 12) 6]
        [(= n 13) 8]
        [(= n 14) 8]
        [(= n 15) 7]
        [(= n 16) 7]
        [(= n 17) 9]
        [(= n 18) 8]
        [(= n 19) 8]
        [(= n 20) 6]
        [(= n 30) 6]
        [(= n 40) 5]
        [(= n 50) 5]
        [(= n 60) 5]
        [(= n 70) 7]
        [(= n 80) 6]
        [(= n 90) 6]
        [(< n 100) (+ (count-number-letters (* 10 (quotient n 10))) (count-number-letters (remainder n 10)))]
        [(and (< n 1000) (= (remainder n 100) 0)) (+ (count-number-letters (quotient n 100)) 7)]
        [(< n 1000) (+ (count-number-letters (* 100 (quotient n 100))) (+ (count-number-letters (remainder n 100)) 3))]
        [(= n 1000) 11]))

(define (count-number-letters-seq n)
  (cond [(< n 1) null]
        [else (cons (count-number-letters n) (count-number-letters-seq (- n 1)))]))


(define result (foldl1 + (count-number-letters-seq 1000)))


(display (~a "Result: " result))
(newline)
