;;; Kakuro puzzle solver

;; Imports
(load "matrix.scm")
(load "utilisp.scm")
(load "backtrack.scm")


;; ===================
;; === RESTRICTION ===
;; ===================

;; define restrict cells in matrix
(define (make-restriction col row)
  (list '*R* col row))

(define (restriction? restr)
  (and (pair? restr) (eq? (car restr) '*R*)))

;; get the sum of column in restriction
(define (restriction-col restr)
  (cadr restr))

;; get the sum of row in restriction
(define (restriction-row restr)
  (caddr restr))


;; ==============
;; === BOARDS ===
;; ==============

;; Return a Kakuro board
(define (kakuro-ref n)
  (define r make-restriction)
  (cond ((= n 0)
         (list->matrix
           `((,(r 0 0)  ,(r 27 0) ,(r 15 0) ,(r 13 0) ,(r 35 0) ,(r 0 0))
             (,(r 0 28) 0         0         0         0         ,(r 12 0))
             (,(r 0 16) 0         0         0         0         0)
             (,(r 0 14) 0         0         ,(r 9 8)  0         0)
             (,(r 0 19) 0         0         0         0         0)
             (,(r 0 0)  ,(r 0 26) 0         0         0         0))))
        ((= n 1)
         (list->matrix
           `((,(r 0 0)  ,(r 8 0)  ,(r 11 0)  ,(r 37 0) ,(r 0 0)  ,(r 0 0)  ,(r 0 0)   ,(r 14 0) ,(r 6 0))
             (,(r 0 24) 0         0          0         ,(r 0 0)  ,(r 0 0)  ,(r 39 12) 0         0       )
             (,(r 0 9)  0         0          0         ,(r 22 0) ,(r 29 9) 0          0         0       )
             (,(r 0 0)  ,(r 0 34) 0          0         0         0         0          0         0       )
             (,(r 0 0)  ,(r 0 0)  ,(r 0 13)  0         0         0         0          ,(r 0 0)  ,(r 0 0))
             (,(r 0 0)  ,(r 14 0) ,(r 22 30) 0         0         0         0          ,(r 7 0)  ,(r 0 0))
             (,(r 0 30) 0         0          0         0         0         0          0         ,(r 9 0))
             (,(r 0 23) 0         0          0         ,(r 0 0)  ,(r 0 20) 0          0         0       )
             (,(r 0 6)  0         0          ,(r 0 0)  ,(r 0 0)  ,(r 0 8)  0          0         0       ))))
        (else (exit n))))


;; ===================
;; === FILL KAKURO ===
;; ===================

;; apply initial pruning
(define (restriction-fill! k)
  (restriction-fill-rows! k)
  (restriction-fill-cols! k))

;; fill row-major possibilities
(define (restriction-fill-rows! k)
  (matrix-for-each-pos-in-col
    (lambda (i _)
      (matrix-row-set! k i (restricted-sequence (split-list (matrix-row k i))
                                                restriction-row)))
    k 0))

;; fill column-major possibilities
(define (restriction-fill-cols! k)
  (matrix-for-each-pos-in-row
    (lambda (_ j)
      (matrix-col-set! k j (restricted-sequence (split-list (matrix-col k j))
                                                restriction-col)))
    k 0))

;; pick restrictions
(define (restricted-sequence seq restr-get)
  (cond ((null? seq) '())
        ((= (restr-get (caar seq)) 0)
          (cons (caar seq)
                (restricted-sequence (cdr seq) restr-get)))
        (else
          (append (cons (caar seq)
                        (restriction-aware (restr-get (caar seq)) (cdar seq)))
                  (restricted-sequence (cdr seq) restr-get)))))

;; fill sequence with possibles values limiteds by restriction
(define (restriction-aware restr sequence)
  (let ((bounds (return-options restr (length sequence))))
    (let iter ((seq sequence))
      (if (null? seq) '()
          (let ((cell (car seq)))
            (if (pair? cell)
                (cons (intersect-bounds cell bounds) (iter (cdr seq)))
                (cons bounds (iter (cdr seq)))))))))

;; build range defined by the intersection of given bounds
(define (intersect-bounds a b)
  (range (max (car a) (car b))
         (min (cdr a) (cdr b))))

;; make a pair (lo . hi) with the cell possibility bounds
(define (return-options restr n)
  (define (pa-sum a1 an n)
    (/ (* (+ a1 an) n) 2))
  (define higher-ascending (min 9 (- restr (pa-sum 1 (- n 1) (- n 1)))))
  (define lower-descending (max 1 (- restr (pa-sum 9 (- 9 (- n 2)) (- n 1)))))
  (cons (max 1 lower-descending)
        (min higher-ascending 9)))


;; ===========================
;; === KAKURO BACKTRACKING ===
;; ===========================

;; find any ambiguity left to resolve
(define (kakuro-ambiguous? k)
  (matrix-find-pos
    (lambda (i j)
      (let ((cell (matrix-ref k i j)))
        (and (pair? cell) (not (restriction? cell)))))
    k))

;; try some possibility in a specific position of the board, providing a way to
;; backtrack and keep solving with all the other possibilities
(define (kakuro-collapse try board fail position)
  (let* ((i (car position)) (j (cadr position))
         (possibilities (matrix-ref board i j))
         (checkpoint (matrix-copy board))) ; save previous board
    (fix-cell! board i j (car possibilities))
    (try board
         (lambda ()
           ; restore previous board and reduce possibility space
           (matrix-set! checkpoint i j (cdr possibilities))
           (try checkpoint fail)))))

;; fix a cell in the board with respect to the cell in given position
(define (fix-cell! board i j cell)
  (define (purge-kakuro i j)
    (let ((others (matrix-ref board i j)))
      (if (and (list? others) (not (restriction? others)))
          (matrix-set! board i j (delete cell others)))))
  (matrix-set! board i j cell) ;; fix that cell
  (matrix-for-each-pos-in-row purge-kakuro board i)  ;; remove it from the rest of the row
  (matrix-for-each-pos-in-col purge-kakuro board j)) ;; as well as from the rest of the column


;; =====================
;; === KAKURO SOLVER ===
;; =====================

;; verify if kakuro is valid
(define (kakuro-solver k)
  (and (kakuro-solver-row k 0) (kakuro-solver-col k 0)))

;; verify wich restriction by row
(define (kakuro-solver-row k i)
  (if (= i (matrix-length k)) #t
      (if (kakuro-solver-seq (split-list (matrix-row k i)) restriction-row)
          (kakuro-solver-row k (+ i 1))
          #f)))

;;verify wich restriction by column
(define (kakuro-solver-col k j)
  (if (= j (length (matrix-row k 0))) #t
      (if (kakuro-solver-seq (split-list (matrix-col k j)) restriction-col)
          (kakuro-solver-col k (+ j 1))
          #f)))

;; verify if kakuro a line of kakuro is solved
(define (kakuro-solver-seq seq get-restr)
  (call/cc (lambda (return)
    (cond ((null? seq) #t)
          ((let ((sum (get-restr (caar seq)))
                 (cells (cdar seq)))
             (cond ((any null? cells) #f)
                   ((any list? cells) (return #t))
                   (else (or (= sum 0) (= sum (apply + cells))))))
           (kakuro-solver-seq (cdr seq) get-restr))
          (else #f)))))


;; ====================
;; === KAKURO TOOLS ===
;; ====================

;; split list in cells that are list
(define (split-list l)
  (cond ((null? l) '())
        ((restriction? (car l))
          (cons (cons (car l)
                      (split-list-aux (cdr l)))
                (split-list (cdr l))))
        (else (split-list (cdr l)))))

(define (split-list-aux l)
  (if (or (null? l) (restriction? (car l))) '()
      (cons (car l)
            (split-list-aux (cdr l)))))

(define (shuffle-kakuro list)
  (if (restriction? list) list (shuffle list)))

(define (set-kakuro n)
  (let ((kakuro (kakuro-ref n)))
    (restriction-fill! kakuro)
    kakuro))
    ; (matrix-map shuffle-kakuro kakuro)))


(define (main)
  ; ;(matrix-display (fill-possibilits (kakuro-ref 0) 0 0) 0)
  ; (display (split-list '((restriction 0 1) 0 0 0 (restriction 0 2) 0 0)))
  ; (display (length (split-list '((restriction 0 1) 0 0 0 (restriction 0 2) 0 0))))
  ; (display (length (car (split-list '((restriction 0 1) 0 0 0 (restriction 0 2) 0 0)))))
  ; (display "\n")
  ; (display (return-options 5 15 #f))
  ; (display "\n")
  ; (display (return-options 3 20 #t))
  ; (display "\n")
  ; ;(display (intersect-bounds '(2 3 4 5) '(1 2 3)))
  ; (display "\n")
  ; (display (matrix-col (kakuro-ref 0) 1))
  ; (display "\nTeste\n")
  ; (display (split-list (matrix-row (kakuro-ref 0) 5)))
  ; (display "\n")
  ; (display (split-list (matrix-row (kakuro-ref 0) 3)))
  ; (display "\n")
  ; (display (split-list (matrix-row (kakuro-ref 0) 3 2)))
  ; (display "\n")
  ; (display (restricted-sequence (split-list (matrix-row (kakuro-ref 0) 3)) #t))
  ; (display "\nMaybe\n")
  ; (display (restricted-sequence (split-list (matrix-row (kakuro-ref 0) 5)) #t))
  ; (display "\nTest Fill Row\n")
  ; (matrix-display (list->matrix (restriction-fill-rows! (kakuro-ref 0))))
  ; (display "\nTest Fill Columns\n")
  ; ;(display (restricted-sequence (split-list (matrix-col (kakuro-ref 0) 0)) #f))
  ; ;(display (restriction-fill-cols! (kakuro-ref 0)))
  ; (matrix-display (restriction-fill-cols! (kakuro-ref 0)))
  ; (display "\nTest Fill All\n")
  ; (matrix-display (restriction-fill! (kakuro-ref 0)))

  ; ; (display "\nTest Shuffle\n")
  ; ; (matrix-display (matrix-map shuffle-kakuro (restriction-fill! (kakuro-ref 0))))

  ; ; (display "\nTest NextPossible\n")
  ; ; (display (kakuro-next-possible (restriction-fill! (kakuro-ref 0)) 3 2))

  ; (display "\nTest Remove value from list of list\n")
  ; ; (display (restricted-sequence (split-list (matrix-row (kakuro-ref 0) 3)) #t))
  ; (newline)
  ; (display (purge-line (map shuffle-kakuro (restricted-sequence (split-list (matrix-row (kakuro-ref 0) 3)) #t)) '(5 6)))
  ; (newline)
  ; (display "\nremove (5 6) from line 3 column 1\n")
  ; (matrix-display (kakuro-rem-possibles (restriction-fill! (kakuro-ref 0)) 3 1 '(5 6)) )

  ; (display "\nTest Solver\n")
  ; (matrix-display (list->matrix (restriction-fill-rows! (kakuro-ref 1))))
  ; (newline)(newline)
  ; (matrix-display (restriction-fill-cols! (kakuro-ref 1)))
  ; (newline)(newline)
  ; (matrix-display (restriction-fill! (kakuro-ref 1)))
  ; (newline)(newline)

  (matrix-display (set-kakuro 1))
  ; (cond
  ;   ((solve (set-kakuro 1)
  ;           kakuro-solver
  ;           kakuro-ambiguous?
  ;           kakuro-collapse)
  ;    => matrix-display)
  ;   (else (display "Impossible\n")))
)

(main)
