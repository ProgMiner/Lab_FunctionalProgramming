#lang racket

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

(define (generate-xs [step 1])
  (define (gen-list from to)
    (if (> from to) null (cons from (gen-list (+ from step) to))))

  (define (generate-xs first-x)
    (lambda (next-x)
      (let* ([xs (gen-list first-x next-x)]
             [next-x (if (empty? xs) first-x (+ (last xs) step))])
        (cons (generate-xs next-x) xs))))

  generate-xs)

(define (split-seq l) (sequence-map string-split l))
(define (numbers-seq l) (sequence-map (lambda (xs) (map (compose1 inexact->exact string->number) xs)) l))
(define (filter-numbers-seq l) (sequence-filter (lambda (xs) (andmap number? xs)) l))
(define (filter-points-seq l) (sequence-filter (lambda (xs) (= 2 (length xs))) l))
(define (points-seq l) (sequence-map (lambda (xs) (apply cons xs)) l))


(define (display-point p)
  (display (exact->inexact (car p)))
  (display " ")
  (display (exact->inexact (cdr p)))
  (newline))

(define (map-zip f xs)
  (map (lambda (x) (cons x (f x))) xs))

(define (handle-stdin approx generate-xs offset)
  (let ([acc
          (sequence-fold
            (lambda (acc p)
              (let* ([approx (list-ref acc 0)]
                     [generate-xs (list-ref acc 1)]
                     [approx-result (approx p)]
                     [generate-xs-result (generate-xs (if (procedure? approx-result) (car p)
                                                        (- (car p) offset)))])
                (if (procedure? approx-result)
                  (list approx-result generate-xs-result (void))
                  (let ([next-approx (car approx-result)]
                        [next-generate-xs (car generate-xs-result)]
                        [approx-fn (cdr approx-result)]
                        [xs (cdr generate-xs-result)])
                    (for-each display-point (map-zip approx-fn xs))
                    (list next-approx next-generate-xs (cons approx-fn (car p)))))))
            (list approx generate-xs (void))
            ((compose1
               points-seq
               filter-points-seq
               filter-numbers-seq
               numbers-seq
               split-seq) (in-lines)))])
    (if (void? (list-ref acc 2))
      (void)
      (let ([approx-fn (car (list-ref acc 2))]
            [last-x (cdr (list-ref acc 2))]
            [generate-xs (list-ref acc 1)])
      (for-each display-point (map-zip approx-fn (cdr (generate-xs last-x))))))))


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

(handle-stdin (approximation-function) (generate-xs (approximation-step)) (approximation-offset))
