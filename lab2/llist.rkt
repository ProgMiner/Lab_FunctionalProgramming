#lang racket

(provide
  llist
  llist-get
  llist-null
  llist-cons
  list->llist
  llist-null?
  llist-head
  llist-tail
  llist-length
  llist-ref
  llist-filter
  llist-map
  llist-foldl
  llist-foldr
  llist->list
  llist-for-each
  llist-take
  llist-drop
  llist-append)


; data LList a = LCons (() -> (a, LList a)) | LNull (() -> ())
;
; llist-get :: LList a -> () -> (a, LList a)
(struct llist (get))

; llist-null :: LList a
(define llist-null (llist (lambda () '())))

; llist-cons :: a -> LList a -> LList a
(define (llist-cons x xs)
  (llist (lambda () (cons x xs))))

; list->llist :: [a] -> LList a
(define (list->llist l)
  (foldr (lambda (x acc) (llist-cons x acc)) llist-null l))

; llist-comp :: LList a -> (a, LList a)
(define (llist-comp l)
  ((llist-get l)))

; llist-null? :: LList a -> Bool
(define llist-null? (compose1 null? llist-comp))

; llist-head :: LList a -> a
(define llist-head (compose1 car llist-comp))

; llist-tail :: LList a -> LList a
(define llist-tail (compose1 cdr llist-comp))

; llist-length :: LList a -> Int
(define (llist-length l)
  (define (llist-length-acc a l)
    (let ([v (llist-comp l)])
      (if (null? v)
        a
        (llist-length-acc (+ a 1) (cdr v)))))
  (llist-length-acc 0 l))

; llist-ref :: LList a -> Int -> a
(define (llist-ref l i)
  (let ([v (llist-comp l)])
    (cond [(= i 0) (car v)]
          [(> i 0) (llist-ref (cdr v) (- i 1))])))

; llist-filter :: (a -> Bool) -> LList a -> LList a
(define (llist-filter f l)
  (llist
    (lambda ()
      (let ([v (llist-comp l)])
        (if (null? v)
          null
          (let ([t (llist-filter f (cdr v))])
            (if (f (car v))
              (cons (car v) t)
              (llist-comp t))))))))

; llist-map :: (a -> b) -> LList a -> LList b
(define (llist-map f l)
  (llist
    (lambda ()
      (let ([v (llist-comp l)])
        (if (null? v)
          null
          (cons (f (car v)) (llist-map f (cdr v))))))))

; llist-foldl :: (b -> a -> b) -> b -> LList a -> b
(define (llist-foldl f a l)
  (let ([v (llist-comp l)])
    (if (null? v)
      a
      (llist-foldl f (f a (car v)) (cdr v)))))

; llist-foldr :: (a -> b -> b) -> b -> LList a -> b
(define (llist-foldr f a l)
  (let ([v (llist-comp l)])
    (if (null? v)
      a
      (f (car v) (llist-foldr f a (cdr v))))))

; llist->list :: LList a -> [a]
(define (llist->list l)
  (llist-foldr cons null l))

; llist-for-each :: LList a -> ()
(define (llist-for-each f l)
  (let ([v (llist-comp l)])
    (if (null? v)
      (void)
      (begin
        (f (car v))
        (llist-for-each f (cdr v))))))

; llist-take :: LList a -> Int -> LList a
(define (llist-take l n)
  (cond [(= n 0) llist-null]
        [(> n 0) (llist
                   (lambda ()
                     (let ([v (llist-comp l)])
                       (cons (car v) (llist-take (cdr v) (- n 1))))))]))

; llist-drop :: LList a -> Int -> LList a
(define (llist-drop l n)
  (cond [(= n 0) l]
        [(> n 0) (llist-drop (llist-tail l) (- n 1))]))

; llist-append :: LList a -> LList a -> LList a
(define (llist-append a b)
  (cond [(llist-null? a) b]
        [(llist-null? b) a]
        [else (llist
                (lambda ()
                  (let ([v (llist-comp a)])
                    (cons (car v) (llist-append (cdr v) b)))))]))
