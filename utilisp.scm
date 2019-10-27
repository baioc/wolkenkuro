;;; Miscellaneous utility procedures

(load "vector.scm") ;; vector-shuffle!


;; gets the car of a list if its not null, alternatively a default value
(define (maybe-car lst alt)
  (if (null? lst) alt (car lst)))

;; return list with x as its last element, like a "reverse cons"
(define (snoc list x)
  (append list `(,x)))

;; make a randomly shuffled list
(define (shuffle list)
  (let ((vec (list->vector list)))
    (vector-shuffle! vec)
    (vector->list vec)))
