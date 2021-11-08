#lang racket

(require rackunit)
(require rackcheck)

(require "llist.rkt")


(define-property llist->list-is-inverse-of-list->llist
  ([xs (gen:list gen:natural)])
  (check-equal? (llist->list (list->llist xs)) xs))

(define-property llist-append-associative
  ([as (gen:list gen:natural)]
   [bs (gen:list gen:natural)]
   [cs (gen:list gen:natural)])
  (let ([a (list->llist as)]
        [b (list->llist bs)]
        [c (list->llist cs)])
    (check-equal?
      (llist->list (llist-append a (llist-append b c)))
      (llist->list (llist-append (llist-append a b) c)))))

(define-property llist-null-is-neutral-for-append
  ([xs (gen:list gen:natural)])
  (let ([x (list->llist xs)])
    (check-equal?
      (llist->list (llist-append llist-null x))
      xs)

    (check-equal?
      (llist->list (llist-append llist-null x))
      (llist->list (llist-append x llist-null)))))

(check-property llist->list-is-inverse-of-list->llist)
(check-property llist-append-associative)
(check-property llist-null-is-neutral-for-append)
