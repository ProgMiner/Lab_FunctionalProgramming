#lang racket


(require "parsec.rkt")
(require "json.rkt")


(define (main)
  (display "Print json: ")
  (let ([line (read-line (current-input-port) 'any)])
    (if (eof-object? line)
      (display "\nBye!\n")
      (let* ([results (parsec/parse parse-json line)]
             [success (parsec/get-success results)])
        (if (parsec/is-success results)
          (begin
            (display "Result: ")
            (display (car success))
            (newline))
          (begin
            (display "Errors:")
            (newline)
            (parsec/print-errors results)))
        (main)))))

(main)
