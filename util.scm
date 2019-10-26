;; return a list whose last element is x
(define (push lst x)
  (append lst `(,x)))

;; returns the car of a list if not null, alternatively a default value
(define (maybe-car lst alt)
  (if (null? lst) alt (car lst)))

;; return the list ori without any elements in the list rem
(define (purge rem ori)
  ; (define delete remove) ; others' remove <=> Guile's delete
  (if (null? rem) ori
      (purge (cdr rem) (delete (car rem) ori))))

;; split list in cells that are list
(define (split-list l)
  (if (null? l)
    '()
    (if (list? (car l))
      (list (cons (car l) (split-list-aux (cdr l))) (split-list (cdr l)))
      (split-list (cdr l))
    )))

(define (split-list-aux l)
  (if (null? l)
    '()
    (if (list? (car l))
      '()
      (cons (car l) (split-list-aux (cdr l)))
    )))
