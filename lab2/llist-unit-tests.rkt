#lang racket

(require rackunit)
(require rackunit/text-ui)

(require "llist.rkt")


(define (flip2 f) (lambda (a b) (f b a)))


(define fibonacci-list
  (begin
    (letrec [(fibonacci-list-from
               (lambda (a b)
                 (let ([c (+ a b)])
                   (llist-cons c (fibonacci-list-from b c)))))]
      (fibonacci-list-from 1 0))))

(define (llist-comp l)
  (let [(v (llist-value l))]
    (if (procedure? v) (v) v)))

(define test-llist-null
  (test-suite "llist-null"
    (test-case "llist-null is empty"
      (check-equal?
        (llist-comp llist-null)
        null))))

(define test-llist-cons
  (test-suite "llist-cons"
    (test-case "llist-cons appends x"
      (let ([l (llist-cons 'x llist-null)])
        (check-equal?
          (car (llist-comp l))
          'x
          "llist-cons don't appends x")
        (check-eq?
          (cdr (llist-comp l))
          llist-null
          "llist-cons don't save tail")))

    (test-case "llist-cons appends x to infinite list"
      (let ([l (llist-cons 'x fibonacci-list)])
        (check-equal?
          (car (llist-comp l))
          'x
          "llist-cons don't appends x")
        (check-eq?
          (cdr (llist-comp l))
          fibonacci-list
          "llist-cons don't save tail")))))

(define test-llist-from-list
  (test-suite "list->llist"
    (test-case "list->llist can make empty llist"
      (check-equal?
        (llist-comp (list->llist null))
        null))

    (test-case "list->llist can make llist with one element"
      (let ([l (list->llist (list 'test))])
        (check-equal?
          (car (llist-comp l))
          'test
          "list->llist result has bad head")
        (check-equal?
          (llist-comp (cdr (llist-comp l)))
          null
          "list->llist result has bad tail")))

    (test-case "list->llist can make llist with more than one element"
      (let ([l (list->llist (list 'a 'b 'c))])
        (check-equal?
          (car (llist-comp l))
          'a
          "list->llist result has bad first element")
        (check-equal?
          (car (llist-comp (cdr (llist-comp l))))
          'b
          "list->llist result has bad second element")
        (check-equal?
          (car (llist-comp (cdr (llist-comp (cdr (llist-comp l))))))
          'c
          "list->llist result has bad third element")
        (check-equal?
          (llist-comp (cdr (llist-comp (cdr (llist-comp (cdr (llist-comp l)))))))
          null
          "list->llist result has bad end")))))

(define test-llist-null?
  (test-suite "llist-null?"
    (test-case "llist-null? works on any empty list"
      (check-true
        (llist-null? (llist (lambda () null)))
        "llist-null? not working on custom empty llist"))

    (test-case "llist-null? works on non empty list"
      (check-false
        (llist-null? (llist (lambda () (cons 1 llist-null))))
        "llist-null? not working on non empty list"))))

(define test-llist-head
  (test-suite "llist-head"
    (test-case "llist-head returns first element"
      (check-equal?
        (llist-head (list->llist (list 'test '123)))
        'test))

    (test-case "llist-head returns first element of infinite list"
      (check-equal? (llist-head fibonacci-list) 1))))

(define test-llist-tail
  (test-suite "llist-tail"
    (test-case "llist-tail returns all except first element"
      (let ([l (llist-tail (list->llist (list 'test '123)))])
        (check-equal?
          (llist-head l)
          '123
          "llist-tail result first element wrong")

        (check-true
          (llist-null? (cdr (llist-comp l)))
          "llist-tail result has wrong length")))

    (test-case "llist-tail returns tail element of infinite list"
      (check-equal? (llist-head (llist-tail fibonacci-list)) 1))))

(define test-llist-length
  (test-suite "llist-length"
    (test-case "llist-length works on any empty lists"
      (check-true (llist-null? (llist (lambda () null)))))

    (test-case "llist-length works on lists with one element"
      (check-eq?
        (llist-length (list->llist (list 'foo)))
        1))

    (test-case "llist-length works on lists with several elements"
      (check-eq?
        (llist-length (list->llist (list 'test '123 'abcde)))
        3))))

(define test-llist-ref
  (test-suite "llist-ref"
    (test-case "llist-ref works correctly"
      (check-equal?
        (llist-ref (list->llist (list 'lol 'kek 'rofl 'ok 'test 'mem)) 3)
        'ok))

    (test-case "llist-ref works correctly on infinite list"
      (check-equal? (llist-ref fibonacci-list 10) 89))))

(define test-llist-filter
  (test-suite "llist-filter"
    (test-case "llist-filter works on any empty lists"
      (check-true (llist-null? (llist-filter (const #t) (llist (lambda () null))))))

    (test-case "llist-filter produces empty list with constant false predicate"
      (check-true (llist-null? (llist-filter (const #f) (list->llist (list 'a 'b 'c 'd 'e))))))

    (test-case "llist-filter produces source list with constant true predicate"
      (let ([l (llist-filter (const #t) (list->llist (list 'x 'y 'z)))])
        (check-equal?
          (llist-head l)
          'x
          "llist-filter result has bad first element")
        (check-equal?
          (llist-head (llist-tail l))
          'y
          "llist-filter result has bad second element")
        (check-equal?
          (llist-head (llist-tail (llist-tail l)))
          'z
          "llist-filter result has bad third element")
        (check-eq?
          (llist-length l)
          3
          "llist-filter result has bad length")))

    (test-case "llist-filter works on regular data"
      (let ([l (llist-filter (lambda (x) (< x 0)) (list->llist (list 34 -234 1234 0 12 95 -3.2)))])
        (check-equal?
          (llist-head l)
          -234
          "llist-filter result has bad first element")
        (check-equal?
          (llist-head (llist-tail l))
          -3.2
          "llist-filter result has bad second element")
        (check-eq?
          (llist-length l)
          2
          "llist-filter result has bad length")))

    (test-case "llist-filter works on infinite list"
      (let ([l (llist-filter (lambda (x) (= (remainder x 2) 0)) fibonacci-list)])
        (check-equal?
          (llist-head l)
          2
          "llist-filter result has bad first element")
        (check-equal?
          (llist-head (llist-tail l))
          8
          "llist-filter result has bad second element")))))

(define test-llist-map
  (test-suite "llist-map"
    (test-case "llist-map works on any empty lists"
      (check-true (llist-null? (llist-map void (llist (lambda () null))))))

    (test-case "llist-map works on regular data"
      (let ([l (llist-map symbol->string (list->llist (list 'x 'y 'u)))])
        (check-equal?
          (llist-head l)
          "x"
          "llist-map result has bad first element")
        (check-equal?
          (llist-head (llist-tail l))
          "y"
          "llist-map result has bad second element")
        (check-equal?
          (llist-head (llist-tail (llist-tail l)))
          "u"
          "llist-map result has bad third element")
        (check-eq?
          (llist-length l)
          3
          "llist-map result has bad length")))

    (test-case "llist-map works on infinite list"
      (let ([l (llist-map number->string fibonacci-list)])
        (check-equal?
          (llist-head l)
          "1"
          "llist-map result has bad first element")
        (check-equal?
          (llist-head (llist-tail l))
          "1"
          "llist-map result has bad second element")
        (check-equal?
          (llist-head (llist-tail (llist-tail l)))
          "2"
          "llist-map result has bad third element")))))

(define test-llist-foldl
  (test-suite "llist-foldl"
    (test-case "llist-foldl works on any empty lists"
      (check-equal?
        (llist-foldl void 'test (llist (lambda () null)))
        'test))

    (test-case "llist-foldl works on regular data"
      (let ([l (list 1 2 3 4 5)])
        (check-equal?
          (llist-foldl (flip2 cons) null (list->llist l))
          (reverse l))))))

(define test-llist-foldr
  (test-suite "llist-foldr"
    (test-case "llist-foldr works on any empty lists"
      (check-equal?
        (llist-foldr void 'test (llist (lambda () null)))
        'test))

    (test-case "llist-foldr works on regular data"
      (let ([l (list 1 2 3 4 5)])
        (check-equal?
          (llist-foldr cons null (list->llist l))
          l)))))

(define test-llist-to-list
  (test-suite "llist->list"
    (test-case "llist->list works on any empty lists"
      (check-equal?
        (llist->list (llist (lambda () null)))
        null))

    (test-case "llist->list works on regular data"
      (let ([l (list 1 2 3 4 5)])
        (check-equal?
          (llist->list (list->llist l))
          l)))))

(define test-llist-for-each
  (test-suite "llist-for-each"
    (test-case "llist-for-each works on any empty lists"
      (let ([a 'abc])
        (check-eq?
          (llist-for-each (lambda (x) (set! a 'xyz)) (llist (lambda () null)))
          (void)
          "llist-for-each returns non void value")
        (check-equal?
          a
          'abc
          "llist-for-each called function on empty list")))

    (test-case "llist-for-each works on regular data"
      (let ([l (list 1 2 3 4 5)]
            [xs null])
        (llist-for-each
          (lambda (x) (set! xs (cons x xs)))
          (list->llist l))
        (check-equal?
          (reverse xs)
          l
          "llist-for-each don't call function properly")))))

(define test-llist-take
  (test-suite "llist-take"
    (test-case "llist-take works on any empty lists"
      (check-true (llist-null? (llist-take (llist (lambda () null)) 0))))

    (test-case "llist-take produces empty list when n = 0"
      (check-true (llist-null? (llist-take (list->llist (list 1 2 3 4 5)) 0))))

    (test-case "llist-take works on regular data"
      (let ([l (list 1 2 3 4 5)])
        (check-equal?
          (llist->list (llist-take (list->llist l) 3))
          (take l 3))))

    (test-case "llist-take works on infinite list"
      (check-equal?
        (llist->list (llist-take fibonacci-list 10))
        (list 1 1 2 3 5 8 13 21 34 55)))))

(define test-llist-drop
  (test-suite "llist-drop"
    (test-case "llist-drop works on any empty lists"
      (check-true (llist-null? (llist-drop (llist (lambda () null)) 0))))

    (test-case "llist-drop produces source list when n = 0"
      (let ([l (list 1 2 3 4 5)])
        (check-equal?
          (llist->list (llist-drop (list->llist l) 0))
          l)))

    (test-case "llist-drop works on regular data"
      (let ([l (list 1 2 3 4 5)])
        (check-equal?
          (llist->list (llist-drop (list->llist l) 3))
          (drop l 3))))

    (test-case "llist-drop works on infinite list"
      (check-equal? (llist-head (llist-drop fibonacci-list 10)) 89))))

(define test-llist-append
  (test-suite "llist-append"
    (test-case "llist-append works on any empty lists"
      (check-true (llist-null? (llist-append (llist (lambda () null)) (llist (lambda () null))))))

    (test-case "llist-append produces a when b is empty"
      (let ([l (list 1 2 3 4 5)])
        (check-equal?
          (llist->list (llist-append (list->llist l) (llist (lambda () null))))
          l)))

    (test-case "llist-append produces b when a is empty"
      (let ([l (list 'q 'w 'e 'r 't 'y)])
        (check-equal?
          (llist->list (llist-append (llist (lambda () null)) (list->llist l)))
          l)))

    (test-case "llist-append works on infinite list"
      (let ([l (list 'q 'w 'e 'r 't 'y)])
        (check-equal?
          (llist->list (llist-take (llist-append (list->llist l) fibonacci-list) (length l)))
          l
          "llist-append cannot append list and infinite list")

        (check-equal?
          (llist->list (llist-take (llist-append fibonacci-list (list->llist l)) 10))
          (list 1 1 2 3 5 8 13 21 34 55)
          "llist-append cannot append list and infinite list")))))


(run-tests test-llist-null)
(run-tests test-llist-cons)
(run-tests test-llist-from-list)
(run-tests test-llist-null?)
(run-tests test-llist-head)
(run-tests test-llist-tail)
(run-tests test-llist-length)
(run-tests test-llist-ref)
(run-tests test-llist-filter)
(run-tests test-llist-map)
(run-tests test-llist-foldl)
(run-tests test-llist-foldr)
(run-tests test-llist-to-list)
(run-tests test-llist-for-each)
(run-tests test-llist-take)
(run-tests test-llist-drop)
(run-tests test-llist-append)
