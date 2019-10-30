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
    (block-prune! board i j (car possibilities))
    (try board
         (lambda ()
           ; restore previous board and reduce possibility space
           (matrix-set! checkpoint i j (cdr possibilities))
           (try checkpoint fail)))))

;; prune the board with respect to the cell in given position
(define (block-prune! board y x cell)
  ;; fix that cell
  (matrix-set! board y x cell)
  ;; remove it from the rest of the row
  (matrix-for-each-pos-in-row
    (lambda (i j)
      (let ((others (matrix-ref board i j)))
        (if (list? others)
            (matrix-set! board i j
              (filter (lambda (value) (not (= value cell))) others)))))
    board y)
  ;; as well as from the rest of the column
  (matrix-for-each-pos-in-col
    (lambda (i j)
      (let ((others (matrix-ref board i j)))
        (if (list? others)
            (matrix-set! board i j
              (filter (lambda (value) (not (= value cell))) others)))))
    board x))

;; puzzle solver
(define (wolkenkratzer n upper left bottom right lo hi)
  ;; check if a board stands candidate to solve the problem
  (define (may-allow? board)
    ;; no blank spaces
    (if (matrix-find-pos (lambda (i j)
                           (null? (matrix-ref board i j)))
                         board)
        #f
        ;; respects all of the puzzle's constraints
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
  (define w (make-matrix n n (range lo hi)))
  ;; pruning board
  (wolkenkratzer-prune w n upper left bottom right)
  ;; actually solving it
  (solve (matrix-map shuffle w)
         may-allow? find-amb-terrain consider-construction))

;; prune wolkenkratzer based in all visualskyscraper
(define (wolkenkratzer-prune mat n upper left bottom right)
  (prune-vs mat n upper  #t + 0) ;row 0
  (prune-vs mat n left   #f + 0) ;column 0
  (prune-vs mat n bottom #t - (- n 1)) ;row n-1
  (prune-vs mat n right  #f - (- n 1)) ;column n-1
)

;; prune wolkenkratzer based in a espedifc visualskyscraper
(define (prune-vs mat n vs iscol op b)
  (if (null? vs)
    '()
    (begin 
      (cond 
        ((= (caar vs) 1) (prune-begin mat n (cdar vs) iscol b)) ;; if the tip is 1
        ((= (caar vs) n) (prune-line mat n (cdar vs) iscol op b)) ;; if tip is the max number
        (else (prune-aux mat
                         (- (caar vs) 1)
                         (range (- n (- (caar vs) 2)) n)
                         (cdar vs)
                         iscol
                         op
                         b))
      )
      (prune-vs mat n (cdr vs) iscol op b)
    )
  )
)

;; define n or the 0 and n option in case of tip = 1
(define (prune-begin mat n pos-aux iscol b)
  ;;define index of cell
  (define pos (if iscol (cons b pos-aux) (cons pos-aux b)))
  (define cell (matrix-ref mat (car pos) (cdr pos)))
  (if (list? cell)
    (if (= (car (matrix-ref mat (car pos) (cdr pos))) 0)
      (matrix-set! mat (car pos) (cdr pos) (list 0 n))
      (block-prune! mat (car pos) (cdr pos) n)))
)
  
;; fill how line start in 1 and goes to n
(define (prune-line mat n pos-aux iscol op b)
  (define aux 1) ;; aux that will help in fill col/row
  (let iter ((i b))
    ;;define index of cell
    (define pos (if iscol (cons i pos-aux) (cons pos-aux i)))
      (if (< i n)
          (begin
            (block-prune! mat (car pos) (cdr pos) aux) ;;fill cell with value
            (set! aux (+ aux 1)) ;;change aux to next value
            (iter (op i 1)))))
)

;; pruning in matrix based in tip 'n'
(define (prune-aux mat n rem-list a-pos iscol op b)
  ;;define index of cell
  (define pos (if iscol (cons b a-pos) (cons a-pos b)))
  (if (= n 0)
    #f
    (begin
      (matrix-set! mat (car pos) (cdr pos)  (purge rem-list (matrix-ref mat (car pos) (cdr pos))))
      (prune-aux mat (- n 1) (cdr rem-list) a-pos iscol op (op b 1)))))

;; format input restrictions
(define (restriction-view-map seq)
  (filter (lambda (pair) (not (= (car pair) 0)))
          (map cons seq (range 0 (- (length seq) 1)))))

;; return the list ori without any elements in the list rem
(define (purge rem ori)
  (if (list? ori)
  ; (define delete remove) ; others' remove <=> Guile's delete
    (if (null? rem) ori
        (purge (cdr rem) (delete (car rem) ori)))
    ori
  )
)

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
    (newline)
    (newline)
    ;(display (range (- 5 (- 2 2)) 5))
 
    ;(wolkenkratzer-prune (make-matrix n n (range lo hi)) n upper left bottom right)
    ))

(main)
(exit 0)
