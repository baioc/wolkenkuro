;;; Implementation of Kakuro board game in Scheme

;; Imports
(load "matrix.scm")
(load "utilisp.scm")
(load "backtrack.scm")

;; define restrict cells in matrix
;; this define cell have this layout
(define (make-restriction col row)
  (list '*R* col row))

;; get the sum of column in restriction
(define (restriction-col restr)
  (cadr restr))

;; get the sum of row in restriction
(define (restriction-row restr)
  (caddr restr))

(define (restriction? restr)
  (and (pair? restr) (eq? (car restr) '*R*)))

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
            (,(r 0 0)  ,(r 0 26) 0         0         0         0)))
        )))

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

;;pick next position of cell that have more then one possibilit0
(define (kakuro-next-possible k i j)
  (if (= i (array-length k))
    #f
    (if (= j (length (matrix-row k 0)))
      (kakuro-next-possible k (+ i 1) 0)
      (if (= (length (matrix-ref k i j)) 1)
        (kakuro-next-possible k i (+ j 1))
        (if (restriction? (matrix-ref k i j))
            (kakuro-next-possible k i (+ j 1))
            (list i j)
        )
      )
    )
  )
)

(define (ambiguous? k)
  (matrix-find-pos
    (lambda (i j)
      (let ((cell (matrix-ref k i j)))
        (and (pair? cell) (not (restriction? cell)))))
    k))

;;remove from colum i and row j, elements in rem from possibles cells
(define (kakuro-rem-possibles k i j rem)
  (matrix-col-set! k j (purge-line (matrix-col k j) rem))
  (matrix-row-set! k i (purge-line (matrix-row k i) rem))
  k
)

;; verify if kakuro a line of kakuro is solved
(define (kakuro-solver-line line get-restr)
  (cond ((null? line) => #t)
        ((let ((sum (get-restr (caar line)))
               (cells (cdar line)))
           (cond ((any null? cells) #f)
                 ((any list? cells) 'skip)
                 (else (or (= sum 0) (= sum (apply + cells))))))
         => (lambda (answer)
              (if (eq? 'skip) 'skip
                  (kakuro-solver-line (cdr line) get-restr))))
        (else #f)))

;;verify wich restriction by column
(define (kakuro-solver-col k j)
  (if (= j (length (matrix-row k 0))) #t
      (let ((answer (kakuro-solver-line (split-list (matrix-col k j))
                                        restriction-col)))
        (cond ((eq? answer 'skip) 'skip)
              (answer (kakuro-solver-col k (+ j 1)))
              (else #f)))))

;; verify wich restriction by row
(define (kakuro-solver-row k i)
  (if (= i (array-length k))
      (let ((answer (kakuro-solver-line (split-list (matrix-row k i))
                                        restriction-row)))
        (cond ((eq? answer 'skip) 'skip)
              (answer (kakuro-solver-row k (+ i 1)))
              (else #f)))))

;; verify if kakuro is valid
(define (kakuro-solver k)
  (let ((first (kakuro-solver-row k 0)))
    (if (eq? first 'skip) #t
        (kakuro-solver-col k 0))))

;; try some possibility in a specific position of the board, providing a way to
;; backtrack and keep solving with all the other possibilities
(define (collapse try board fail position)
  (let* ((i (car position)) (j (cadr position))
         (possibilities (matrix-ref board i j))
         (checkpoint (matrix-copy board))) ; save previous board
    (display "fixing ") (display (list (car possibilities))) (display `(,i ,j)) (newline)
    (matrix-display board) (newline) (newline)
    (if (fix-cell! board i j (car possibilities))
        (try board
             (lambda ()
               ; restore previous board and reduce possibility space
               (matrix-set! checkpoint i j (cdr possibilities))
               (try checkpoint fail)))
        (fail)))

;; fix a cell in the board with respect to the cell in given position
(define (fix-cell! board y x cell)
  (call/cc (lambda (return)
    (define (purge-kakuro i j)
        (let ((others (matrix-ref board i j)))
          (if (and (list? others) (not (restriction? others)))
              (let ((reduced (delete cell others)))
                (matrix-set! board i j reduced)
                (if (null? reduced) (return #f))))))
    ;; fix that cell
    (matrix-set! board y x cell)
    ;; remove it from the rest of the row
    (matrix-for-each-pos-in-row purge-kakuro board y)
    ;; as well as from the rest of the column
    (matrix-for-each-pos-in-col purge-kakuro board x))))

(define (main)
  ; ;(matrix-display (fill-possibilits (make-kakuro 0) 0 0) 0)
  ; (display (split-list '((restriction 0 1) 0 0 0 (restriction 0 2) 0 0)))
  ; (display (length (split-list '((restriction 0 1) 0 0 0 (restriction 0 2) 0 0))))
  ; (display (length (car (split-list '((restriction 0 1) 0 0 0 (restriction 0 2) 0 0)))))
  ; (display "\n")
  ; (display (return-options 5 15 #f))
  ; (display "\n")
  ; (display (return-options 3 20 #t))
  ; (display "\n")
  ; ;(display (correct-val '(2 3 4 5) '(1 2 3)))
  ; (display "\n")
  ; (display (matrix-col (make-kakuro 0) 1))
  ; (display "\nTeste")
  ; (display (caaar (split-list (matrix-row (make-kakuro 0) 5))))
  ; (display "\n")
  ; (display (split-list (matrix-row (make-kakuro 0) 3)))
  ; (display "\n")
  ; (display (split-list (matrix-row (make-kakuro 0) 3 2)))
  ; (display "\n")
  ; (display (fill-in-restr (split-list (matrix-row (make-kakuro 0) 3)) #t))
  ; (display "\nMaybe\n")
  ; (display (fill-in-restr (split-list (matrix-row (make-kakuro 0) 5)) #t))
  ; (display "\nTest Fill Row\n")
  ; (matrix-display (list->matrix (fill-row-kakuro (make-kakuro 0))))
  ; (display "\nTest Fill Columns\n")
  ; ;(display (fill-in-restr (split-list (matrix-col (make-kakuro 0) 0)) #f))
  ; ;(display (fill-col-kakuro (make-kakuro 0)))
  ; (matrix-display (fill-col-kakuro (make-kakuro 0)))
  (display "\nTest Fill All\n")
  (matrix-display (prune-kakuro (make-kakuro 0)))

  ; (display "\nTest Shuffle\n")
  ; (matrix-display (matrix-map shuffle-kakuro (prune-kakuro (make-kakuro 0))))

  ; (display "\nTest NextPossible\n")
  ; (display (kakuro-next-possible (prune-kakuro (make-kakuro 0)) 3 2))

  (display "\nTest Remove value from list of list\n")
  ; (display (fill-in-restr (split-list (matrix-row (make-kakuro 0) 3)) #t))
  (newline)
  (display (purge-line (map shuffle-kakuro (fill-in-restr (split-list (matrix-row (make-kakuro 0) 3)) #t)) '(5 6)))
  (newline)
  (display "\nremove (5 6) from line 3 column 1\n")
  (matrix-display (kakuro-rem-possibles (prune-kakuro (make-kakuro 0)) 3 1 '(5 6)) )

  (display "\nTest Solver\n")
  (matrix-display
    (solve (set-kakuro 0)
      kakuro-solver
      ambiguous?
      collapse))
)

(define (set-kakuro n)
  (matrix-map shuffle-kakuro (prune-kakuro (make-kakuro n))))

(main)
