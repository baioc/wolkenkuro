;;; Implementation of Kakuro board game in Scheme

;; Imports
(load "util.scm")
(load "matrix.scm")

;; define restrict cells in matrix
;; this define cell have this layout
;;('restriction (sum in column) (sum in row) (quantity of cells in column) (quantity of cells in row))
(define (make-restriction col row)
  (list 'restriction col row))

;; get the sum of column in restriction
(define (restriction-sum-column restr)
  (cadr restr))

;; get the sum of row in restriction
(define (restriction-sum-row restr)
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



;; fill cell with options values
(define (prune-line line list)
  line
)

;; Pruning a line in kakuro
;;pick line, sum and the qt of cells, remove numbers of possibilits
;;that exceed the sum
(define (prune-line line n)
  n
)

(define (main)
  (show-matrix (fill-possibilits (make-kakuro 0) 0 0) 0)
  (display (split-list '('(rest) 0 0 0 '(o sa ) 0 0)))
  (display (length (split-list '('(rest) 0 0 0 '(o sa ) 0 0))))
  (display (length (car (split-list '('(rest) 0 0 0 '(o sa ) 0 0)))))
)
(main)
