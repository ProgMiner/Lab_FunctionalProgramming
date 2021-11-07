#lang racket


(define (count-number-letters-plus n acc)
  (cond [(= n 1) (+ acc (string-length "one"))]
        [(= n 2) (+ acc (string-length "two"))]
        [(= n 3) (+ acc (string-length "three"))]
        [(= n 4) (+ acc (string-length "four"))]
        [(= n 5) (+ acc (string-length "five"))]
        [(= n 6) (+ acc (string-length "six"))]
        [(= n 7) (+ acc (string-length "seven"))]
        [(= n 8) (+ acc (string-length "eight"))]
        [(= n 9) (+ acc (string-length "nine"))]
        [(= n 10) (+ acc (string-length "ten"))]
        [(= n 11) (+ acc (string-length "eleven"))]
        [(= n 12) (+ acc (string-length "twelve"))]
        [(= n 13) (+ acc (string-length "thirteen"))]
        [(= n 14) (+ acc (string-length "fourteen"))]
        [(= n 15) (+ acc (string-length "fifteen"))]
        [(= n 16) (+ acc (string-length "sixteen"))]
        [(= n 17) (+ acc (string-length "seventeen"))]
        [(= n 18) (+ acc (string-length "eighteen"))]
        [(= n 19) (+ acc (string-length "nineteen"))]
        [(= n 20) (+ acc (string-length "twenty"))]
        [(= n 30) (+ acc (string-length "thirty"))]
        [(= n 40) (+ acc (string-length "forty"))]
        [(= n 50) (+ acc (string-length "fifty"))]
        [(= n 60) (+ acc (string-length "sixty"))]
        [(= n 70) (+ acc (string-length "seventy"))]
        [(= n 80) (+ acc (string-length "eighty"))]
        [(= n 90) (+ acc (string-length "ninety"))]
        [(< n 100) (count-number-letters-plus (* 10 (quotient n 10)) (+ (count-number-letters (remainder n 10)) acc))]
        [(and (< n 1000) (= (remainder n 100) 0)) (count-number-letters-plus (quotient n 100) (+ 7 acc))]
        [(< n 1000) (count-number-letters-plus (* 100 (quotient n 100)) (+ (count-number-letters (remainder n 100)) (+ 3 acc)))]
        [(= n 1000) (+ 11 acc)]))

(define (count-number-letters n)
  (count-number-letters-plus n 0))

(define (sum-number-letters-acc n acc)
  (cond [(< n 1) acc]
        [else (sum-number-letters-acc (- n 1) (+ (count-number-letters n) acc))]))

(define (sum-number-letters n)
  (sum-number-letters-acc n 0))


(define result (sum-number-letters 1000))


(display (~a "Result: " result))
(newline)
