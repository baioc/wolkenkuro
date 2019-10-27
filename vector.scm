;;; Useful vector operations

;; unary map for vectors (not in Guile by default)
(define (vector-map proc vec)
  (vector-map-pos (lambda (idx) (proc (vector-ref vec idx))) vec))

;; index-based vector version of map
(define (vector-map-pos proc vec)
  (let* ((n (vector-length vec))
         (new (make-vector n)))
    (let iter ((idx 0))
      (if (< idx n)
          (begin
            (vector-set! new idx (proc idx))
            (iter (+ idx 1)))))
    new))

;; finds the first index of a vector that satisfies a predicate, else #f
(define (vector-find-pos pred vec)
  (let for ((k 0) (n (vector-length vec)))
    (cond ((= k n) #f)
          ((pred k) k)
          (else (for (+ k 1) n)))))

;; checks if a vector index satisfies a predicate for every valid index
(define (vector-every-pos pred vec)
  (not (vector-find-pos (lambda (idx) (not (pred idx))) vec)))

;; Fisher-Yates-Knuth in-place shuffle
(define (vector-shuffle! vec)
  (let iter ((i (- (vector-length vec) 1)))
    (if (> i 0)
        (let ((j (random (+ i 1))))
          (if (not (= i j))
            (let ((tmp (vector-ref vec i)))
              (vector-set! vec i (vector-ref vec j))
              (vector-set! vec j tmp)))
          (iter (- i 1))))))
