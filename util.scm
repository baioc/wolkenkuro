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
