#lang racket


(provide
  parsec/pos
  parsec/pos-col
  parsec/pos-row
  parsec/error-tail
  parsec/error-pos
  parsec/error-message
  parsec/result-tail
  parsec/result-pos
  parsec/result-data
  parsec/pos>?
  parsec/pos->string
  parsec/parse
  parsec/is-success
  parsec/get-success
  parsec/get-errors
  parsec/print-errors
  parsec/fmap
  parsec/pure
  parsec/apply
  parsec/empty
  parsec/alt
  parsec/some
  parsec/many
  parsec/return
  parsec/bind
  parsec/chain
  parsec/do
  parsec/define-do
  parsec/define-token
  parsec/any
  parsec/char-if
  parsec/char
  parsec/string
  parsec/whitespaces
  parsec/some-sep
  parsec/many-sep
  parsec/exact
  parsec/unsigned-integer-number
  parsec/numeric-sign
  parsec/integer-number
  parsec/plain-floating-number
  parsec/exponential-floating-number
  parsec/number
  parsec/trace)


; data Pos = {
;     row :: Integer,
;     col :: Integer,
; }
(struct parsec/pos (row col) #:transparent)

; data Error = {
;     tail :: String,
;     pos :: Pos,
;     message :: String,
; }
(struct parsec/error (tail pos message) #:transparent)

; data Result a = {
;     tail :: String,
;     pos :: Pos,
;     data :: a,
; }
(struct parsec/result (tail pos data) #:transparent)

; newtype Parser a = { parse :: (Pos, String) -> [result] }
;   result = Result a   -- for result
;          | Error      -- for error
(struct parsec/parser (parse))


; Utils

(define (parsec/pos-inc pos c)
  (if (char=? #\newline c)
    (parsec/pos (+ (parsec/pos-row pos) 1) 0)
    (parsec/pos (parsec/pos-row pos) (+ (parsec/pos-col pos) 1))))

(define (parsec/pos-append pos str)
  (foldl (lambda (c acc) (parsec/pos-inc acc c)) pos (string->list str)))

(define (parsec/pos>? a b)
  (let ([a-row (parsec/pos-row a)]
        [a-col (parsec/pos-col a)]
        [b-row (parsec/pos-row b)]
        [b-col (parsec/pos-col b)])
    (if (= a-row b-row)
      (> a-col b-col)
      (> a-row b-row))))

(define (parsec/pos->string pos)
  (string-append
    (number->string (parsec/pos-row pos))
    ":"
    (number->string (parsec/pos-col pos))))

(define (parsec/map-results f results)
  (map
    (lambda (result)
      (if (parsec/result? result)
        (f (parsec/result-tail result) (parsec/result-pos result) (parsec/result-data result))
        result))
    results))

(define (parsec/flat-map-results f results)
  (apply append
    (map
      (lambda (result)
        (if (parsec/result? result)
          (f (parsec/result-tail result) (parsec/result-pos result) (parsec/result-data result))
          (list result)))
      results)))

(define (parsec/parse parser str [pos (parsec/pos 1 0)])
  (define p (parsec/parser-parse parser))
  (p pos str))

(define (parsec/is-success results [many-results #f])
  (let ([success-results (filter parsec/result? results)])
    (if many-results
      (not (empty? success-results))
      (= (length success-results) 1))))

(define (parsec/get-success results)
  (filter parsec/result? results))

(define (parsec/remove-duplicated-errors errors)
  (reverse
    (foldl
      (lambda (e errs)
        (cond
          [(empty? errs) (list e)]
          [(equal? e (car errs)) errs]
          [else (cons e errs)]))
      null
      errors)))

(define (parsec/get-errors results)
  (let* ([errors (sort (filter parsec/error? results) parsec/pos>? #:key parsec/error-pos)]
         [max-pos (parsec/error-pos (car errors))]
         [last-errors (filter (lambda (e) (equal? (parsec/error-pos e) max-pos)) errors)]
         [last-errors-sorted (sort last-errors string<? #:key parsec/error-message)]
         [uniq-errors (parsec/remove-duplicated-errors last-errors-sorted)])
    uniq-errors))

(define (parsec/print-errors results)
  (let ([errors (parsec/get-errors results)])
    (for-each
      (lambda (e)
        (display
          (string-append
            "- At "
            (parsec/pos->string (parsec/error-pos e))
            " parsing error: "
            (parsec/error-message e)
            "\n")))
      errors)))


; instance Functor Parser where

(define (parsec/fmap f parser)
  (define (convert results)
    (parsec/map-results
      (lambda (tail pos data) (parsec/result tail pos (f data)))
      results))

  (parsec/parser
    (let ([p1 (parsec/parser-parse parser)])
      (lambda (pos str) (convert (p1 pos str))))))

; instance Applicative Parser where

(define (parsec/pure x) (parsec/parser (lambda (pos str) (list (parsec/result str pos x)))))

(define (parsec/apply parser1 parser2)
  (parsec/parser
    (lambda (pos str)
      (let* ([p1 (parsec/parser-parse parser1)]
             [p2 (parsec/parser-parse parser2)]
             [results1 (p1 pos str)])
        (parsec/map-results
          (lambda (r1-tail r1-pos r1-data)
            (let ([results2 (p2 r1-pos r1-tail)])
              (parsec/map-results
                (lambda (r2-tail r2-pos r2-data)
                  (parsec/result r2-tail r2-pos (r1-data r2-data)))
                results2)))
          results1)))))

; instance Alternative Parser where

(define parsec/empty (parsec/parser (lambda (pos str) null)))

(define (parsec/alt parser1 parser2)
  (let ([p1 (parsec/parser-parse parser1)]
        [p2 (parsec/parser-parse parser2)])
    (parsec/parser (lambda (pos str) (append (p1 pos str) (p2 pos str))))))

(define (parsec/some parser)
  (define p (parsec/parser-parse parser))

  (define (some-p pos str)
    (let ([results (p pos str)])
      (append
        (parsec/map-results
          (lambda (tail pos data) (parsec/result tail pos (list data)))
          results)
        (parsec/flat-map-results
          (lambda (tail pos data)
            (parsec/map-results
              (lambda (tail pos data1) (parsec/result tail pos (cons data data1)))
              (some-p pos tail)))
          (filter parsec/result? results)))))

  (parsec/parser some-p))

(define (parsec/many parser) (parsec/alt (parsec/pure null) (parsec/some parser)))

; instance Monad Parser where

(define parsec/return parsec/pure)

(define (parsec/bind parser f)
  (define p (parsec/parser-parse parser))

  (parsec/parser
    (lambda (pos str)
      (let ([results (p pos str)])
        (parsec/flat-map-results
          (lambda (tail pos data)
            (let* ([parser1 (f data)]
                   [p1 (parsec/parser-parse parser1)]
                   [results1 (p1 pos tail)])
              results1))
          results)))))

(define-syntax-rule (parsec/chain parser1 parser2)
  (parsec/bind parser1 (lambda (val) parser2)))

; Syntax

(define (parsec/do-wrap expr)
  (if (parsec/parser? expr) expr (parsec/return expr)))

; Example:
; (parsec/do
;   key <- parse-key
;   (parsec/string "=")
;   value <- parse-value
;   (cons key value))
(define-syntax parsec/do
  (syntax-rules (<-)
    [(_) (void)]
    [(_ expr) (parsec/do-wrap expr)]
    [(_ name <- expr exprs ...)
     (parsec/bind (parsec/do-wrap expr) (lambda (name) (parsec/do exprs ...)))]
    [(_ expr exprs ...) (parsec/chain (parsec/do-wrap expr) (parsec/do exprs ...))]))

(define-syntax-rule (parsec/define-do name stmts ...)
  (define name (parsec/do stmts ...)))

(define-syntax-rule (parsec/define-token name value)
  (parsec/define-do name
    parsec/whitespaces
    (parsec/string value)))

(define-syntax parsec/any
  (syntax-rules ()
    [(_ parser) parser]
    [(_ parser parsers ...) (parsec/alt parser (parsec/any parsers ...))]))

; Basic parsers

(define (parsec/char-if pred)
  (parsec/parser
    (lambda (pos str)
      (if (string=? str "")
        (list (parsec/error str pos "unexpected EOT"))
        (let ([h (string-ref str 0)])
          (if (pred h)
            (list (parsec/result (substring str 1) (parsec/pos-inc pos h) h))
            (list (parsec/error str pos (string-append
                                          "unexpected character "
                                          (~s (string h)))))))))))

(define (parsec/char c)
  (let ([err-tail (string-append "expected " (~s (string c)))])
    (parsec/parser
      (lambda (pos str)
        (if (string=? str "")
          (list (parsec/error str pos (string-append "unexpected EOT, " err-tail)))
          (let ([h (string-ref str 0)])
            (if (char=? h c)
              (list (parsec/result (substring str 1) (parsec/pos-inc pos h) h))
              (list (parsec/error str pos (string-append
                                            "unexpected character "
                                            (~s (string h))
                                            ", "
                                            err-tail))))))))))

(define (parsec/string s)
  (define s-len (string-length s))

  (define (starts-with str)
    (if (< (string-length str) s-len)
      #f
      (string=? (substring str 0 s-len) s)))

  (let ([err-tail (string-append "expected " (~s s))])
    (parsec/parser
      (lambda (pos str)
        (if (starts-with str)
          (list (parsec/result (substring str s-len) (parsec/pos-append pos s) s))
          (list (parsec/error str pos (string-append
                                        "unexpected string "
                                        (~s (substring str 0 (min s-len (string-length str))))
                                        ", "
                                        err-tail))))))))

(define parsec/whitespaces (parsec/many (parsec/char-if char-whitespace?)))

(define (parsec/some-sep parser sep)
  (parsec/do
    head <- parser
    tail <- (parsec/alt
              (parsec/do
                sep
                (parsec/some-sep parser sep))
              (parsec/pure null))
    (cons head tail)))

(define (parsec/many-sep parser sep)
  (parsec/alt
    (parsec/pure null)
    (parsec/some-sep parser sep)))

(define (parsec/exact parser num)
  (if (= num 0)
    (parsec/pure null)
    (parsec/do
      head <- parser
      tail <- (parsec/exact parser (- num 1))
      (cons head tail))))

(define parsec/digit (parsec/char-if (lambda (c) (char<=? #\0 c #\9))))

(parsec/define-do parsec/unsigned-integer-number
  digits <- (parsec/some parsec/digit)
  (string->number (list->string digits)))

(define parsec/numeric-sign
  (parsec/any
    (parsec/do (parsec/char #\-) -1)
    (parsec/do (parsec/char #\+) 1)
    (parsec/pure 1)))

(parsec/define-do parsec/integer-number
  sign <- parsec/numeric-sign
  uint <- parsec/unsigned-integer-number
  (* sign uint))

(parsec/define-do parsec/plain-floating-number
  int <- (parsec/alt parsec/integer-number (parsec/pure 0))
  (parsec/char #\.)
  frac <- parsec/unsigned-integer-number
  str <- (string-append (number->string int) "." (number->string frac))
  (string->number str 10 'number-or-false 'decimal-as-exact))

(parsec/define-do parsec/exponential-floating-number
  num <- (parsec/any parsec/plain-floating-number parsec/integer-number)
  (parsec/any (parsec/char #\e) (parsec/char #\E))
  exp <- parsec/integer-number
  str <- (string-append (number->string num) "e" (number->string exp))
  (string->number str 10 'number-or-false 'decimal-as-exact))

(define parsec/number
  (parsec/any
    parsec/integer-number
    parsec/plain-floating-number
    parsec/exponential-floating-number))

; Debug

(define (parsec/trace prefix)
  (parsec/parser
    (lambda (pos str)
      (display
        (string-append
          prefix
          " Pos: ("
          (number->string (parsec/pos-row pos))
          ", "
          (number->string (parsec/pos-col pos))
          "). Input: "
          (~s str)
          "\n"))
      (list (parsec/result str pos null)))))
