;;; Useful vector operations

;; unary map for vectors (not in Guile by default)
(define (vector-map proc vec)
  (list->vector (map proc (vector->list vec))))

;; finds the first index of a vector that satisfies a predicate, else #f
(define (vector-find-pos pred vec)
  (let for ((k 0) (n (vector-length vec)))
    (cond ((= k n) #f)
          ((pred k) k)
          (else (for (+ k 1) n)))))

;; checks if a vector index satisfies a predicate for every valid index
(define (vector-every-pos pred vec)
  (not (vector-find-pos (lambda (idx) (not (pred idx))) vec)))
