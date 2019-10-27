;;; Matrices and their manipulation as contiguous data structures
;;; uses Guile arrays but a vector implementation is provided in array.scm

(load "utilisp.scm") ;; maybe-car, snoc


;; make a m*n matrix filled with a default value v
(define (make-matrix m n . opt-v)
  (make-array (maybe-car opt-v #f) m n))

;; get the jth element from ith row of a matrix
(define (matrix-get mat i j)
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


;; get the ith row of a matrix as a list (in order), starting from column j|0
(define (matrix-row mat i . opt-j)
  (let* ((dim (array-dimensions mat))
         (n (cadr dim)))
    (let iter ((row '()) (idx (maybe-car opt-j 0)))
      (if (>= idx n) row
          (iter (snoc row (matrix-get mat i idx)) (+ idx 1))))))

;; get the jth column of a matrix as a list (in order), starting from row i|0
(define (matrix-col mat j . opt-i)
  (let* ((dim (array-dimensions mat))
         (m (car dim)))
    (let iter ((col '()) (idx (maybe-car opt-i 0)))
      (if (>= idx m) col
          (iter (snoc col (matrix-get mat idx j)) (+ idx 1))))))

(define (matrix-col-set! mat j column . opt-i)
  (if (null? column)
    '()
    (if (eq? (caar column) 'restriction)
      (matrix-col-set! mat j (cdr column) (+ (maybe-car opt-i 0) 1))
      (begin 
        (matrix-set! mat (maybe-car opt-i 0) j (car column))
        (matrix-col-set! mat j (cdr column) (+ (maybe-car opt-i 0) 1))
      )
    )
    ))

;; print matrix in monitor
(define (show-matrix mat . opt-i)
  (if (= (maybe-car opt-i 0) (array-length mat))
    0
    (begin
      (display (matrix-row mat (maybe-car opt-i 0)))
      (display "\n")
      (show-matrix mat (+ (maybe-car opt-i 0) 1))
    )))
