(define (matrix-get m i j)
  (vector-ref (vector-ref m i) j))

(define (matrix-set! m i j x)
  (vector-set! (vector-ref m i) j x))

(define (remove-possibilities ori rem)
  (if (null? rem) ori
      (remove-possibilities (remv (car rem) ori) (cdr rem))))

(define (matrix-map proc m)
  (vector-map (lambda (line) (vector-map proc line)) m))

(define (make-possibilities m n)
  (make-vector m (make-vector n '(1 2 3 4 5 6 7 8 9))))
