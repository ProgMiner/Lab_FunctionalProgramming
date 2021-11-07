#lang racket


(define-syntax my-cond
  (syntax-rules (if)
    [(_ _) (void)]
    [(_ v [if cond value] cont ...) (if cond value (my-cond v cont ...))]
    [(_ v [n name] cont ...) (if (= v n) (string-length name) (my-cond v cont ...))]))


(define (count-number-letters n)
  (my-cond n
    [1 "one"]
    [2 "two"]
    [3 "three"]
    [4 "four"]
    [5 "five"]
    [6 "six"]
    [7 "seven"]
    [8 "eight"]
    [9 "nine"]
    [10 "ten"]
    [11 "eleven"]
    [12 "twelve"]
    [13 "thirteen"]
    [14 "fourteen"]
    [15 "fifteen"]
    [16 "sixteen"]
    [17 "seventeen"]
    [18 "eighteen"]
    [19 "nineteen"]
    [20 "twenty"]
    [30 "thirty"]
    [40 "forty"]
    [50 "fifty"]
    [60 "sixty"]
    [70 "seventy"]
    [80 "eighty"]
    [90 "ninety"]
    [if (< n 100) (+ (count-number-letters (* 10 (quotient n 10))) (count-number-letters (remainder n 10)))]
    [if (and (< n 1000) (= (remainder n 100) 0)) (+ (count-number-letters (quotient n 100)) 7)]
    [if (< n 1000) (+ (count-number-letters (* 100 (quotient n 100))) (+ (count-number-letters (remainder n 100)) 3))]
    [1000 "onethousand"]))

(define (sum-number-letters n)
  (cond [(< n 1) 0]
        [else (+ (count-number-letters n) (sum-number-letters (- n 1)))]))


(define result (sum-number-letters 1000))


(display (~a "Result: " result))
(newline)
