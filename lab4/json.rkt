#lang racket

(provide parse-json)

(require "parsec.rkt")


(parsec/define-token LBRACE "{")
(parsec/define-token RBRACE "}")
(parsec/define-token LBRACKET "[")
(parsec/define-token RBRACKET "]")
(parsec/define-token COMMA ",")
(parsec/define-token COLON ":")
(parsec/define-token DOUBLE_QUOTE "\"")
(parsec/define-token TRUE "true")
(parsec/define-token FALSE "false")
(parsec/define-token NULL "null")

(parsec/define-do (parse-json-factory)
  (parsec/any
    parse-json-object
    parse-json-array
    parse-json-string
    parse-json-number
    parse-json-boolean
    parse-json-null))

(parsec/define-do (parse-json-object-key-value)
  key <- parse-json-string
  COLON
  value <- parse-json
  (cons key value))

(parsec/define-do parse-json-object
  LBRACE
  content <- (parsec/many-sep (parse-json-object-key-value) COMMA)
  RBRACE
  content)

(parsec/define-do parse-json-array
  LBRACKET
  content <- (parsec/many-sep parse-json COMMA)
  RBRACKET
  content)

(define (is-regular-json-char c)
  (not
    (or
      (char<? c #\u0020)
      (char=? #\" c)
      (char=? #\\ c))))

(define (is-hex-char c)
  (or
    (char<=? #\0 c #\9)
    (char<=? #\a c #\f)
    (char<=? #\A c #\F)))

(parsec/define-do parse-hex-char (parsec/char-if is-hex-char))

(parsec/define-do parse-json-string-char
  (parsec/any
    (parsec/char-if is-regular-json-char)
    (parsec/do
      (parsec/char #\\)
      (parsec/any
        (parsec/char #\")
        (parsec/char #\\)
        (parsec/char #\/)
        (parsec/char #\b)
        (parsec/char #\f)
        (parsec/char #\n)
        (parsec/char #\r)
        (parsec/char #\t)
        (parsec/do
          (parsec/char #\u)
          hex <- (parsec/exact parse-hex-char 4)
          (integer->char (string->number (list->string hex) 16)))))))

(parsec/define-do parse-json-string
  DOUBLE_QUOTE
  content <- (parsec/many parse-json-string-char)
  DOUBLE_QUOTE
  (list->string content))

(parsec/define-do parse-json-number
  parsec/whitespaces
  parsec/number)

(parsec/define-do parse-json-boolean
  str <- (parsec/any TRUE FALSE)
  (if (string=? str "true") #t #f))

(parsec/define-do parse-json-null
  NULL
  null)

(define parse-json (parse-json-factory))
