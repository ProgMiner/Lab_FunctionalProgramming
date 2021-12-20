#lang racket

(require racket/generator)


(define (linear-approximate first-point)
  (lambda (next-point)
    (cons (linear-approximate next-point)
          (lambda (x)
            (let-values ([(xa ya) (values (car first-point) (cdr first-point))]
                         [(xb yb) (values (car next-point) (cdr next-point))])
              (+ (/ (* (- x xa) (- yb ya)) (- xb xa)) ya))))))

(define (lagrange-approximate first-point)
  (define (approx ps)
    ; Ln(x) = sum(y_i * prod((x - x_j) / (x_i - x_j), j = 0..n, j ≠ i), i = 0..n)
    (lambda (x)
      (foldl
        (lambda (p-i sum)
          (+
            sum
            (*
              (cdr p-i)
              (foldl
                (lambda (p-j prod)
                  (*
                    prod
                    (/
                      (- x (car p-j))
                      (- (car p-i) (car p-j)))))
                1
                (filter
                  (lambda (p-j) (not (= (car p-i) (car p-j))))
                  ps)))))
        0
        ps)))
  (define (next-fn p ps)
    (let ([ps1 (cons p ps)])
      (cons
        (lambda (p1) (next-fn p1 ps1))
        (approx ps1))))

  (lambda (next-point) (next-fn next-point (cons first-point null))))

(define (generate-xs step)
  (define (generate-from x-from)
    (lambda (x-to)
      (let* ([xs (inclusive-range x-from x-to step)]
             ; if we cannot generate any point at this moment
             ; we would like to start with current state next time
             [next-x-from (if (empty? xs) x-from (+ (last xs) step))])
        (cons (generate-from next-x-from) xs))))

  generate-from)

(define (split-seq l) (sequence-map string-split l))
(define (numbers-seq l) (sequence-map (lambda (xs) (map (compose1 inexact->exact string->number) xs)) l))
(define (filter-numbers-seq l) (sequence-filter (lambda (xs) (andmap number? xs)) l))
(define (filter-points-seq l) (sequence-filter (lambda (xs) (= 2 (length xs))) l))
(define (points-seq l) (sequence-map (lambda (xs) (apply cons xs)) l))


(define (map-zip f xs)
  (map (lambda (x) (cons x (f x))) xs))

(define (handle-stdin approx generate-xs offset)
  ; fold state
  ;
  ; approx - next approx function
  ; generate-xs - next xs generation function
  ; prev-iteration - result of previous approximation step,
  ;                  pair of confiugred approx function and x of last consumed point
  (struct state (approx generate-xs prev-iteration))

  (define (handle-point st p)
    (let* ([approx-result ((state-approx st) p)]
           [next-x-to (if (procedure? approx-result) (car p) (- (car p) offset))]
           [generate-xs-result ((state-generate-xs st) next-x-to)])

      ; if approx factory returned procedure instead of pair,
      ; it still is not configured for approximation (e.g. after first point)
      ; and we cannot produce output
      (if (procedure? approx-result)
        (state approx-result generate-xs-result (void))
        (let ([next-approx (car approx-result)]
              [next-generate-xs (car generate-xs-result)]
              [approx-fn (cdr approx-result)]
              [xs (cdr generate-xs-result)])
          (for-each yield (map-zip approx-fn xs))
          (state next-approx next-generate-xs (cons approx-fn (car p)))))))

  (define process-stdin
    (compose1
      points-seq
      filter-points-seq
      filter-numbers-seq
      numbers-seq
      split-seq))

  (in-generator
    (let ([st (sequence-fold
                handle-point
                (state approx generate-xs (void))
                (process-stdin (in-lines)))])

      ; if we made enough steps to achieve some configured approx function,
      ; we need to approximate tail points, that could remain if offset > 0
      (unless (void? (state-prev-iteration st))
        (let* ([approx-fn (car (state-prev-iteration st))]
               [last-x (cdr (state-prev-iteration st))]
               [last-xs (cdr ((state-generate-xs st) last-x))])
          (for-each yield (map-zip approx-fn last-xs)))))))

(define (display-point p)
  (display (exact->inexact (car p)))
  (display " ")
  (display (exact->inexact (cdr p)))
  (newline))


(define approximation-function (make-parameter linear-approximate))
(define approximation-step (make-parameter 1))
(define approximation-offset (make-parameter 0))

(command-line
  #:once-each
  [("-s" "--step") step
    "minimal step of points to approximation, defaults to 1"
    (approximation-step (inexact->exact (string->number step)))]

  [("-o" "--offset") offset
    "offset for delayed approximation, defaults to 0"
    (approximation-offset (inexact->exact (string->number offset)))]

  [("-a" "--algorithm") algorithm
    "approximation algorithm from: linear, lagrange; defaults to linear"
    (approximation-function (cond
                              [(equal? algorithm "lagrange") lagrange-approximate]
                              [(equal? algorithm "linear") linear-approximate]
                              [else (raise "unknown approximation algorithm" #t)]))]

  #:args () (void))

(sequence-for-each display-point
  (handle-stdin (approximation-function) (generate-xs (approximation-step)) (approximation-offset)))
