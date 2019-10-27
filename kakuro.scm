;;; Implementation of Kakuro board game in Scheme

;; Imports
(load "matrix.scm")
(load "utilisp.scm")

;; define restrict cells in matrix
;; this define cell have this layout
;;('restriction (sum in column) (sum in row) (quantity of cells in column) (quantity of cells in row))
(define (make-restriction col row)
  (list 'restriction col row))

;; get the sum of column in restriction
(define (restriction-col restr)
  (cadr restr))

;; get the sum of row in restriction
(define (restriction-row restr)
  (caddr restr))

;; get the qt of cells in column restriction
;(define (restriction-qt-column restr)
;  (cadddr restr))

;; get the qt of cells in row restriction
;(define (restriction-qt-row restr)
;  (car (cddddr restr)))

;; Return a Kakuro board
(define (make-kakuro n)
  (define r make-restriction)
  (cond ((= n 0)
        (list->matrix 
          `((,(r 0 0)  ,(r 27 0) ,(r 15 0) ,(r 13 0) ,(r 35 0) ,(r 0 0))
            (,(r 0 28) 0         0         0         0         ,(r 12 0))
            (,(r 0 16) 0         0         0         0         0)
            (,(r 0 14) 0         0         ,(r 9 8)  0         0)
            (,(r 0 19) 0         0         0         0         0)
            (,(r 0 0)  ,(r 0 16) 0         0         0         0)))
        )))

;; fill cells with options
(define (fill-possibilits k i j)
  (if (= i (length (matrix-row k 0)))
    k
    (if (= j (array-length k))
      (fill-possibilits k (+ i 1) 0)
      (if (list? (matrix-get k i j))
        (fill-possibilits k i (+ j 1))
        (begin 
          (matrix-set! k i j '(1 2 3 4 5 6 7 8 9))
          (fill-possibilits k i (+ j 1))
        )
      )
    )
  )
)

;; fill possibilits in row of kakuro
(define (fill-row-kakuro k . opt-i)
  (if (= (maybe-car opt-i 0) (length (matrix-row k 0)))
    '()
    (append (list (fill-in-restr (split-list (matrix-row k (maybe-car opt-i 0))) #t))
      (fill-row-kakuro k (+ (maybe-car opt-i 0) 1)))
  )
)

(define (fill-col-kakuro k . opt-j)
  (if (= (maybe-car opt-j 0) (length (matrix-col k 0)))
    ;'()
    ;(append (list (fill-in-restr (split-list (matrix-col k (maybe-car opt-j 0))) #f))
    ;  (fill-col-kakuro k (+ (maybe-car opt-j 0) 1)))
    k
    (begin 
      (matrix-col-set! k (maybe-car opt-j 0)
        (fill-in-restr (split-list (matrix-col k (maybe-car opt-j 0))) #f))
      (fill-col-kakuro k (+ (maybe-car opt-j 0) 1)))
  )
)
;; fill line with possibles values limiteds by restriction
(define (fill-possibles-restr restr line)
  (if (= restr 0)
    line
    (if (null? line)
      line
      (if (> (/ restr (length line)) 5) ;; pick mean, sum of restriction by qt of cells
        (fill-cells line (return-options (length line) restr #t))
        (fill-cells line (return-options (length line) restr #f))
      )
    )
  )
)

(define (fill-cells line val)
  (if (null? line)
    '()
    (cons (correct-val (car line) val) (fill-cells (cdr line) val))
  )
)

;; pick restrictions
(define (fill-in-restr line isrow)
  (if (null? line)
    '()
    (if isrow
      (if (= (restriction-row (caar line)) 0)
        (cons (caar line) (fill-in-restr (cdr line) isrow))
        (append (cons (caar line) (fill-possibles-restr (restriction-row (caar line)) (cdar line))) (fill-in-restr (cdr line) isrow))
      )
      (if (= (restriction-col (caar line)) 0)
        (cons (caar line) (fill-in-restr (cdr line) isrow))
        (append (cons (caar line) (fill-possibles-restr (restriction-col (caar line)) (cdar line))) (fill-in-restr (cdr line) isrow))
      )
    )
  )
)

;; Pruning a line in kakuro
;;pick line, sum and the qt of cells, remove numbers of possibilits
;;that exceed the sum
(define (prune-kakuro k)
  (list->matrix (fill-row-kakuro (fill-col-kakuro k)))
)

(define (kakuro-next-possible k i j)
  (if (= i (array-length k))
    #f
    (if (= j (length (matrix-row k 0)))
      (kakuro-next-possible k (+ i 1) 0)
      (if (= (length (matrix-get k i j)) 1)
        (kakuro-next-possible k i (+ j 1))
        (if (eq? (car (matrix-get k i j)) 'restriction)
          (kakuro-next-possible k i (+ j 1))
          (list i j)
        )
      )
    )
  )
)

(define (main)
  (show-matrix (fill-possibilits (make-kakuro 0) 0 0) 0)
  (display (split-list '((restriction 0 1) 0 0 0 (restriction 0 2) 0 0)))
  (display (length (split-list '((restriction 0 1) 0 0 0 (restriction 0 2) 0 0))))
  (display (length (car (split-list '((restriction 0 1) 0 0 0 (restriction 0 2) 0 0)))))
  (display "\n")
  (display (return-options 5 15 #f))
  (display "\n")
  (display (return-options 3 20 #t))
  (display "\n")
  ;(display (correct-val '(2 3 4 5) '(1 2 3)))
  (display "\n")
  (display (matrix-col (make-kakuro 0) 1))
  (display "\nTeste")
  (display (caaar (split-list (matrix-row (make-kakuro 0) 5))))
  (display "\n")
  (display (split-list (matrix-row (make-kakuro 0) 3)))
  (display "\n")
  (display (split-list (matrix-row (make-kakuro 0) 3 2)))
  (display "\n")
  (display (fill-in-restr (split-list (matrix-row (make-kakuro 0) 3)) #t))
  (display "\nMaybe\n")
  (display (fill-in-restr (split-list (matrix-row (make-kakuro 0) 5)) #t))
  (display "\nTest Fill Row\n")
  (show-matrix (list->matrix (fill-row-kakuro (make-kakuro 0))))
  (display "\nTest Fill Columns\n")
  ;(display (fill-in-restr (split-list (matrix-col (make-kakuro 0) 0)) #f))
  ;(display (fill-col-kakuro (make-kakuro 0)))
  (show-matrix (fill-col-kakuro (make-kakuro 0)))
  (display "\nTest Fill All\n")
  (show-matrix (prune-kakuro (make-kakuro 0)))

  (display "\nTest Shuffle\n")
  (show-matrix (matrix-map shuffle-kakuro (prune-kakuro (make-kakuro 0))))

  (display "\nTest NextPossible\n")
  (display (kakuro-next-possible (prune-kakuro (make-kakuro 0)) 3 2))

)
(main)
