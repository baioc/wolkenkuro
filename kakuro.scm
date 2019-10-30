;;; Kakuro puzzle solver

;; Imports
(load "matrix.scm")
(load "utilisp.scm")
(load "backtrack.scm")


;; ==============
;; === BOARDS ===
;; ==============

;; Return some test-case-specific Kakuro board
(define (kakuro-ref n)
  (define r make-restriction)
  (cond ((= n 0)
         (list->matrix ;; https://www.janko.at/Raetsel/Kakuro/283.a.htm
           `((,(r 0 0)  ,(r 27 0) ,(r 15 0) ,(r 13 0) ,(r 35 0) ,(r 0 0))
             (,(r 0 28) 0         0         0         0         ,(r 12 0))
             (,(r 0 16) 0         0         0         0         0)
             (,(r 0 14) 0         0         ,(r 9 8)  0         0)
             (,(r 0 19) 0         0         0         0         0)
             (,(r 0 0)  ,(r 0 26) 0         0         0         0))))
        ((= n 1)
         (list->matrix ;; https://www.janko.at/Raetsel/Kakuro/091.a.htm
           `((,(r 0 0)  ,(r 12 0) ,(r 21 0)  ,(r 0 0)   ,(r 16 0) ,(r 13 0))
             (,(r 0 17) 0         0          ,(r 22 11) 0         0        )
             (,(r 0 15) 0         0          0          0         0        )
             (,(r 0 0)  ,(r 4 13) 0          0          0         ,(r 10 0))
             (,(r 0 18) 0         0          0          0         0        )
             (,(r 0 10) 0         0         ,(r 0 14)   0         0        ))))
        ((= n 2)
         (list->matrix ;; https://www.janko.at/Raetsel/Kakuro/246.a.htm
           `((,(r 0 0)  ,(r 8 0)  ,(r 11 0)  ,(r 37 0) ,(r 0 0)  ,(r 0 0)  ,(r 0 0)   ,(r 14 0) ,(r 6 0))
             (,(r 0 24) 0         0          0         ,(r 0 0)  ,(r 0 0)  ,(r 39 12) 0         0       )
             (,(r 0 9)  0         0          0         ,(r 22 0) ,(r 29 9) 0          0         0       )
             (,(r 0 0)  ,(r 0 34) 0          0         0         0         0          0         0       )
             (,(r 0 0)  ,(r 0 0)  ,(r 0 13)  0         0         0         0          ,(r 0 0)  ,(r 0 0))
             (,(r 0 0)  ,(r 14 0) ,(r 22 30) 0         0         0         0          ,(r 7 0)  ,(r 0 0))
             (,(r 0 30) 0         0          0         0         0         0          0         ,(r 9 0))
             (,(r 0 23) 0         0          0         ,(r 0 0)  ,(r 0 20) 0          0         0       )
             (,(r 0 6)  0         0          ,(r 0 0)  ,(r 0 0)  ,(r 0 8)  0          0         0       ))))
        ((= n 3)
         (list->matrix ;; https://www.janko.at/Raetsel/Kakuro/287.a.htm
           `((,(r 0 0)  ,(r 0 0)   ,(r 18 0) ,(r 8 0)  ,(r 12 0)  ,(r 11 0)  ,(r 0 0)   ,(r 37 0) ,(r 12 0))
             (,(r 0 0)  ,(r 24 29) 0         0         0          0          ,(r 0 10)  0         0        )
             (,(r 0 15) 0          0         0         0          0          ,(r 10 17) 0         0        )
             (,(r 0 9)  0          0         ,(r 0 0)  ,(r 0 11)  0          0          0         0        )
             (,(r 0 6)  0          0         ,(r 10 0) ,(r 23 13) 0          0          0         ,(r 13 0))
             (,(r 0 21) 0          0         0         0          ,(r 0 0)   ,(r 7 10)  0         0        )
             (,(r 0 0)  ,(r 4 0)   ,(r 5 8)  0         0          ,(r 12 11) 0          0         0        )
             (,(r 0 32) 0          0         0         0          0          0          0         ,(r 0 0) )
             (,(r 0 6)  0          0         0         ,(r 0 8)   0          0          ,(r 0 0)  ,(r 0 0) ))))
        (else (exit n))))


;; ====================
;; === RESTRICTIONS ===
;; ====================

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
  (let* ((free (- n 1))
         (higher-ascending (min 9 (- restr (pa-sum 1 free free))))
         (lower-descending (max 1 (- restr (pa-sum 9 (- 9 (- free 1)) free)))))
    (cons (max 1 lower-descending)
          (min higher-ascending 9))))


;; =====================
;; === KAKURO SOLVER ===
;; =====================

;; verify if kakuro is valid
(define (kakuro-solver k)
  ;; no blank spaces
  (and (not (matrix-find-pos
              (lambda (i j) (null? (matrix-ref k i j))) k))
       ;; and both directions check
       (kakuro-solver-row k 0)
       (kakuro-solver-col k 0)))

;; verify wich restriction by row
(define (kakuro-solver-row k i)
  (cond ((= i (matrix-length k)) #t)
        ((kakuro-solver-seq (split-list (matrix-row k i)) restriction-row)
         (kakuro-solver-row k (+ i 1)))
        (else #f)))

;;verify wich restriction by column
(define (kakuro-solver-col k j)
  (cond ((= j (matrix-length k)) #t)
        ((kakuro-solver-seq (split-list (matrix-col k j)) restriction-col)
         (kakuro-solver-col k (+ j 1)))
        (else #f)))

;; verify if kakuro a line of kakuro is solved
(define (kakuro-solver-seq seq get-restr)
  (if (null? seq) #t
      (let ((sum (get-restr (caar seq)))
            (cells (cdar seq)))
        (if (or (= sum 0)
                (any list? cells)
                (= sum (apply + cells)))
            (kakuro-solver-seq (cdr seq) get-restr)
            #f))))


;; ===========================
;; === KAKURO BACKTRACKING ===
;; ===========================

;; find any ambiguity left to resolve
(define (kakuro-ambiguous? k)
  (matrix-find-pos
    (lambda (i j)
      (let ((cell (matrix-ref k i j)))
        (and (not (restriction? cell)) (pair? cell))))
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
(define (fix-cell! board y x cell)
  (define (purge-kakuro-row! i j)
    (if (>= j (matrix-length board)) 'done
        (let ((others (matrix-ref board i j)))
          (if (restriction? others) 'done ; stop on next restriction
              (begin
                (if (list? others)
                    (matrix-set! board i j (delete cell others)))
                (purge-kakuro-row! i (+ j 1)))))))
  (define (purge-kakuro-col! i j)
    (if (>= i (matrix-length board)) 'done
        (let ((others (matrix-ref board i j)))
          (if (restriction? others) 'done ; stop on next restriction
              (begin
                (if (list? others)
                    (matrix-set! board i j (delete cell others)))
                (purge-kakuro-col! (+ i 1) j))))))
  (matrix-set! board y x cell)   ; fix that cell
  (purge-kakuro-row! y (+ x 1))  ; purge repetitions in horizontal restriction cells
  (purge-kakuro-col! (+ y 1) x)) ; purge repetitions in vertical restriction cells


;; ====================
;; === KAKURO TOOLS ===
;; ====================

;; split list in cells that are list
(define (split-list l)
  (cond ((null? l) '())
        ((restriction? (car l))
          (cons (cons (car l) (split-list-aux (cdr l)))
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
    (matrix-map shuffle-kakuro kakuro)))

(define (kakuro-display k)
  (matrix-for-each-pos-in-col
    (lambda (i _)
      (if (not (= i 0)) (display "\n"))
      (matrix-for-each-pos-in-row
        (lambda (_ j)
          (if (not (= j 0)) (display ""))
            (if (not (restriction? (matrix-ref k i j)))
                (begin (display "     ")
                       (display (matrix-ref k i j))
                       (display "      "))
                (begin (display " ")
                       (display (matrix-ref k i j))
                       (display "  "))))
        k i))
    k 0))

(define (solve-kakuro? n)
  (display "\n====== Solving Kakuro Board ") (display n) (display " ======\n\n")
  (cond ((solve (set-kakuro n)
                kakuro-solver
                kakuro-ambiguous?
                kakuro-collapse) => kakuro-display)
        (else (display "Impossible\n")))
  (newline)
  (newline))


;; main program
(define (main)
  ; (solve-kakuro? 0)
  ; (solve-kakuro? 1)
  ; (solve-kakuro? 2)
  (solve-kakuro? 3)
)

(main)
(exit 0)
