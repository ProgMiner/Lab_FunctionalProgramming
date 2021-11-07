#lang racket

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
        [(= n 40) 6]
        [(= n 50) 5]
        [(= n 60) 5]
        [(= n 70) 7]
        [(= n 80) 6]
        [(= n 90) 6]
        [(< n 100) (+ (count-number-letters (* 10 (quotient n 10))) (count-number-letters (remainder n 10)))]
        [(and (< n 1000) (= (remainder n 100) 0)) (+ (count-number-letters (quotient n 100)) 10)]
        [(< n 1000) (+ (count-number-letters (* 100 (quotient n 100))) (count-number-letters (remainder n 100)))]
        [(= n 1000) 8]))

(define result (foldl + 0 (map count-number-letters (range 1 1001))))

(display (~a "Result: " result))
(newline)