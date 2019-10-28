;;; Miscellaneous utility procedures

(load "vector.scm") ;; vector-shuffle!

;; identity function
(define (identity x) x)

;; checks if there exists any element in the list that satisfies a predicate
(define (any pred list)
  (cond ((null? list) #f)
        ((pred (car list)) => identity)
        (else (any pred (cdr list)))))

;; checks if an element x satisfies a predicate for every x in the list
(define (every pred list)
  (not (any (lambda (x) (not (pred x))) list)))

;; similar to iota, but as an inclusive [min..max] range
(define (range lo hi)
  (let iter ((k hi) (seq '()))
    (if (< k lo) seq
        (iter (- k 1) (cons k seq)))))

;; gets the car of a list if its not null, alternatively a default value
(define (maybe-car lst alt)
  (if (null? lst) alt (car lst)))

;; build randomly shuffled list
(define (shuffle list)
  (let ((vec (list->vector list)))
    (vector-shuffle! vec)
    (vector->list vec)))


;; @TODO: check usage

;; tail-recursive L->R fold (not in Guile by default)
(define (fold-left proc init lst)
  (if (null? lst) init
      (fold-left proc (proc init (car lst)) (cdr lst))))

;; return list with x as its last element, like a "reverse cons"
(define (snoc list x)
  (append list `(,x)))
