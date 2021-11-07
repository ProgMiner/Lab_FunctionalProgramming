#lang racket


; (define (foldl1 f xs)
;   (foldl f (car xs) (cdr xs)))
; 
; (define (compose-list fs)
;   (foldl1 compose1 fs))

(define (compose-list fs)
  (apply compose1 fs))


(define (count-number-letters-plus n)
  (cond [(= n 1) (lambda (acc) (+ acc (string-length "one")))]
        [(= n 2) (lambda (acc) (+ acc (string-length "two")))]
        [(= n 3) (lambda (acc) (+ acc (string-length "three")))]
        [(= n 4) (lambda (acc) (+ acc (string-length "four")))]
        [(= n 5) (lambda (acc) (+ acc (string-length "five")))]
        [(= n 6) (lambda (acc) (+ acc (string-length "six")))]
        [(= n 7) (lambda (acc) (+ acc (string-length "seven")))]
        [(= n 8) (lambda (acc) (+ acc (string-length "eight")))]
        [(= n 9) (lambda (acc) (+ acc (string-length "nine")))]
        [(= n 10) (lambda (acc) (+ acc (string-length "ten")))]
        [(= n 11) (lambda (acc) (+ acc (string-length "eleven")))]
        [(= n 12) (lambda (acc) (+ acc (string-length "twelve")))]
        [(= n 13) (lambda (acc) (+ acc (string-length "thirteen")))]
        [(= n 14) (lambda (acc) (+ acc (string-length "fourteen")))]
        [(= n 15) (lambda (acc) (+ acc (string-length "fifteen")))]
        [(= n 16) (lambda (acc) (+ acc (string-length "sixteen")))]
        [(= n 17) (lambda (acc) (+ acc (string-length "seventeen")))]
        [(= n 18) (lambda (acc) (+ acc (string-length "eighteen")))]
        [(= n 19) (lambda (acc) (+ acc (string-length "nineteen")))]
        [(= n 20) (lambda (acc) (+ acc (string-length "twenty")))]
        [(= n 30) (lambda (acc) (+ acc (string-length "thirty")))]
        [(= n 40) (lambda (acc) (+ acc (string-length "forty")))]
        [(= n 50) (lambda (acc) (+ acc (string-length "fifty")))]
        [(= n 60) (lambda (acc) (+ acc (string-length "sixty")))]
        [(= n 70) (lambda (acc) (+ acc (string-length "seventy")))]
        [(= n 80) (lambda (acc) (+ acc (string-length "eighty")))]
        [(= n 90) (lambda (acc) (+ acc (string-length "ninety")))]
        [(< n 100) (compose1
                     (count-number-letters-plus (* 10 (quotient n 10)))
                     (count-number-letters-plus (remainder n 10)))]
        [(and (< n 1000) (= (remainder n 100) 0))
         (lambda (acc) ((count-number-letters-plus (quotient n 100)) (+ 7 acc)))]
        [(< n 1000)
         (lambda (acc)
           ((count-number-letters-plus (* 100 (quotient n 100)))
            ((count-number-letters-plus (remainder n 100)) (+ 3 acc))))]
        [(= n 1000) (lambda (acc) (+ 11 acc))]))

(define (sum-number-letters-plus n)
  (cond [(< n 1) (list (lambda (acc) acc))]
        [else (cons (count-number-letters-plus n) (sum-number-letters-plus (- n 1)))]))

(define (sum-number-letters n)
  ((compose-list (sum-number-letters-plus n)) 0))


(define result (sum-number-letters 1000))


(display (~a "Result: " result))
(newline)
