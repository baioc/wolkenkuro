;;; Matrices and their manipulation as contiguous data structures
;;; uses Guile arrays but a vector implementation is provided in array.scm

(load "utilisp.scm") ;; maybe-car, identity, snoc

;; make a m*n matrix filled with a default value v
(define (make-matrix m n . opt-v)
  (make-array (maybe-car opt-v #f) m n))

;; get the jth element from ith row of a matrix
(define (matrix-ref mat i j)
  (array-ref mat i j))

;; set the jth element from ith row of a matrix to x
(define (matrix-set! mat i j x)
  (array-set! mat x i j))

;; get a new matrix built by applying proc to each element of matrix mat
(define (matrix-map proc mat)
  (let ((new (apply make-array (cons #f (array-dimensions mat)))))
    (array-map! new proc mat)
    new))

;; converts a matrix to a list of lists
(define (matrix->list mat)
  (array->list mat))

;; converts a list of lists to a matrix
(define (list->matrix lst)
  (let ((m (- (length lst) 1))
        (n (- (length (car lst)) 1)))
    (list->array `((0 ,m) (0 ,n)) lst)))

;; shallow copy
(define (matrix-copy mat)
  (matrix-map identity mat))

;; finds the first position that satisfies a predicate, else #f
(define (matrix-find-pos pred mat)
  (let* ((dim (array-dimensions mat))
         (m (car dim)) (n (cadr dim)))
    (let iter ((i 0) (j 0))
      (cond ((= i m) #f)
            ((= j n) (iter (+ i 1) 0))
            ((pred i j) `(,i ,j))
            (else (iter i (+ j 1)))))))

;; checks if every matrix position satisfies a predicate
(define (matrix-every-pos pred mat)
  (not (matrix-find-pos (lambda (i j) (not (pred i j))) mat)))

;; position-based matrix version of map
(define (matrix-map-pos proc mat)
  (let ((mapped (matrix-copy mat)))
    (matrix-find-pos
      (lambda (i j) (matrix-set! mapped i j (proc i j)) #f) mapped)
    mapped))


;; @TODO: check usage

;; get the ith row of a matrix as a list (in order), starting from column j|0
(define (matrix-row mat i . opt-j)
  (let* ((dim (array-dimensions mat))
         (n (cadr dim)))
    (let iter ((row '()) (idx (maybe-car opt-j 0)))
      (if (>= idx n) row
          (iter (snoc row (matrix-ref mat i idx)) (+ idx 1))))))

;; get the jth column of a matrix as a list (in order), starting from row i|0
(define (matrix-col mat j . opt-i)
  (let* ((dim (array-dimensions mat))
         (m (car dim)))
    (let iter ((col '()) (idx (maybe-car opt-i 0)))
      (if (>= idx m) col
          (iter (snoc col (matrix-ref mat idx j)) (+ idx 1))))))

;; print matrix in monitor
(define (show-matrix mat str i)
  (if (= i (length (matrix-row mat 0)))
    str
    (string-append str (matrix-row mat i) "\n"
    (show-matrix mat str (+ i 1)))
  )
)

