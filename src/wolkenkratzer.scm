;;; imports
(load "matrix.scm")
(load "utilisp.scm") ;; every, range, shuffle
(load "backtrack.scm")

;; checks if the skyscrapers seen from function get's POV (indexed in a certain
;; range [from,to) and incremented by unitary steps) comply to a restriction
(define (skyscrapers-check? r i get from step to)
  (let iter ((j from) (count 0) (tallest 0))
    (if (= j to)
        (= count r)
        (let ((curr (get i j)))
          (if (list? curr) 'skip
              (iter (step j 1)
                    (if (> curr tallest) (+ count 1) count)
                    (max tallest curr)))))))

;; find a position with an ambiguity, false if there isn't
(define (find-amb-terrain board)
  (matrix-find-pos (lambda (i j) (list? (matrix-ref board i j))) board))

;; try some possibility in a specific position of the board, providing a way to
;; backtrack and keep solving with all the other possibilities
(define (consider-construction try board fail position)
  (let* ((i (car position)) (j (cadr position))
         (possibilities (matrix-ref board i j))
         (checkpoint (matrix-copy board))) ; save previous board
    (prune-cross! board i j (car possibilities))
    (try board
         (lambda ()
           ; restore previous board and reduce possibility space
           (matrix-set! checkpoint i j (cdr possibilities))
           (try checkpoint fail)))))

;; puzzle solver
(define (wolkenkratzer n upper left bottom right lo hi)
  ; check if a board stands candidate to solve the problem
  (define (may-allow? board)
    ; no blank spaces
    (if (matrix-find-pos (lambda (i j)
                           (null? (matrix-ref board i j)))
                         board)
        #f
        ; respects all of the puzzle's constraints?
        (and (every (lambda (cnstr)
                      (let ((restr (car cnstr)) (row (cdr cnstr)))
                        (skyscrapers-check?
                          restr row
                          (lambda (i j) (matrix-ref board i j))
                          0 + n)))
                    left)
             (every (lambda (cnstr)
                      (let ((restr (car cnstr)) (row (cdr cnstr)))
                        (skyscrapers-check?
                          restr row
                          (lambda (i j) (matrix-ref board i j))
                          (- n 1) - -1)))
                    right)
             (every (lambda (cnstr)
                      (let ((restr (car cnstr)) (col (cdr cnstr)))
                        (skyscrapers-check?
                          restr col
                          (lambda (i j) (matrix-ref board j i))
                          (- n 1) - -1)))
                    bottom)
             (every (lambda (cnstr)
                      (let ((restr (car cnstr)) (col (cdr cnstr)))
                        (skyscrapers-check?
                          restr col
                          (lambda (i j) (matrix-ref board j i))
                          0 + n)))
                    upper))))

  (let ((board (make-matrix n n (range lo hi))))
    ; apply initial pruning
    (wolkenkratzer-prune! board n lo hi upper #t + 0)
    (wolkenkratzer-prune! board n lo hi left #f + 0)
    (wolkenkratzer-prune! board n lo hi bottom #t - (- n 1))
    (wolkenkratzer-prune! board n lo hi right #f - (- n 1))
    ; actually solving it
    (solve (matrix-map (lambda (p) (if (list? p) (shuffle p) p)) board)
           may-allow? find-amb-terrain consider-construction)))

;; prune the board with respect to the cell in given position
(define (prune-cross! board y x cell)
  ; fix that cell
  (matrix-set! board y x cell)
  ; remove it from the rest of the row
  (matrix-for-each-pos-in-row
    (lambda (i j)
      (let ((others (matrix-ref board i j)))
        (if (list? others)
            (matrix-set! board i j
              (filter (lambda (value) (not (= value cell))) others)))))
    board y)
  ; as well as from the rest of the column
  (matrix-for-each-pos-in-col
    (lambda (i j)
      (let ((others (matrix-ref board i j)))
        (if (list? others)
            (matrix-set! board i j
              (filter (lambda (value) (not (= value cell))) others)))))
    board x))

;; prune board based on a specific set of visible skyscrapers restrictions
(define (wolkenkratzer-prune! board n lo hi restrs col? step border)
  ; used when tip is 1: first cell is either {0 hi} or a fixed n
  (define (prune-begin! idx)
    (let* ((i (if col? border idx))
           (j (if col? idx border))
           (cell (matrix-ref board i j)))
      (cond ((not (pair? cell)) 'already-fixed)
            ((= lo 0) (matrix-set! board i j (list 0 hi)))
            (else (prune-cross! board i j n)))))
  ; used when tip is n: the whole sequence is [1..n]
  (define (prune-sequence! idx)
    (let iter ((pos border) (curr 1))
      (if (and (not (< pos 0)) (not (>= pos n)))
          (let ((i (if col? pos idx)) (j (if col? idx pos)))
            (if (not (pair? (matrix-ref board i j)))
                (prune-cross! board i j curr))
            (iter (step pos 1) (+ curr 1))))))
  ; used when tip is k, iterate purging impossible candidates
  (define (prune-rest! k rem-list idx border)
    (if (and (not (null? rem-list))
             (not (< idx 0))
             (not (>= idx n)))
        (let* ((i (if col? border idx))
               (j (if col? idx border))
               (cell (matrix-ref board i j)))
          (if (list? cell)
              (matrix-set! board i j (purge rem-list cell)))
          (prune-rest! (- k 1) (cdr rem-list) idx (step border 1)))))
  ; for each restriction...
  (let iter ((restrs restrs))
    (if (not (null? restrs))
        (begin
          (cond ((= (caar restrs) 1) (prune-begin! (cdar restrs)))
                ((= (caar restrs) n) (prune-sequence! (cdar restrs)))
                (else (prune-rest! (- (caar restrs) 1)
                                   (range (- n (- (caar restrs) 2)) hi)
                                   (cdar restrs)
                                   border)))
          (iter (cdr restrs))))))

;; format input restrictions
(define (restriction-view-map seq)
  (filter (lambda (pair) (not (= (car pair) 0)))
          (map cons seq (range 0 (- (length seq) 1)))))

;; main script
(define (main)
  (let ((upper 'nil) (left 'nil) (bottom 'nil) (right 'nil)
        (n 'nil) (lo 'nil) (hi 'nil))

    (display "Enter upper restrictions (left->right, as a list): ")
    (set! upper (read))
    (set! n (length upper))
    (set! upper (restriction-view-map upper))

    (display "Enter left restrictions (up->down, as a list): ")
    (set! left (restriction-view-map (read)))

    (display "Enter bottom restrictions (left->right, as a list): ")
    (set! bottom (restriction-view-map (read)))

    (display "Enter right restrictions (up->down, as a list): ")
    (set! right (restriction-view-map (read)))

    (display "Enter maximum skyscraper height: ")
    (set! hi (read))
    (set! lo (if (= hi n) 1 0))

    (display "Looking for a solution...\n")
    (cond ((wolkenkratzer n upper left bottom right lo hi) => matrix-display)
          (else (display "Impossible!")))
    (newline)))

(main)
(exit 0)
