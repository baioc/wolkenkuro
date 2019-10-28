;; imports
(load "backtrack.scm") ;; solve
(load "matrix.scm") ;; make-matrix, matrix-ref, matrix-set!, matrix-find-pos, matrix-map-pos
(load "utilisp.scm") ;; every, range

;; example from https://www.janko.at/Raetsel/Wolkenkratzer/072.a.htm
; instance restrictions
(define upper (list (cons 4 0) (cons 1 1)))
(define left (list (cons 3 1) (cons 2 2)))
(define bottom (list (cons 1 0) (cons 2 1) (cons 2 2)))
(define right (list (cons 3 3)))

; board size and cell range
(define n 4)
(define lo 1)
(define hi 4)

;; check if a board stands candidate to solve the problem
(define (possible? board)
  (call/cc (lambda (return)
    ;; no blank spaces
    (if (matrix-find-pos (lambda (i j)
                           (null? (matrix-ref board i j)))
                         board)
        (return #f)
        ;; respects all of the puzzle's constraints
        (and (every (lambda (cnstr)
                      (let* ((restr (car cnstr)) (row (cdr cnstr))
                             (answer (skyscrapers-check?
                                       restr row
                                       (lambda (i j) (matrix-ref board i j))
                                       0 + n)))
                        ;; if there are ambiguities, return true to skip
                        (if (eq? answer 'skip) (return #t) answer)))
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
                    upper))))))

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

;; find a position with an ambiguity, must return false if there isn't any
(define (find-ambiguous board)
  (matrix-find-pos (lambda (i j) (list? (matrix-ref board i j))) board))

;; try some possibility in a specific position of the board, providing a way to
;; backtrack and keep solving with all the other possibilities
(define (collapse try board fail position)
  (let* ((i (car position)) (j (cadr position))
         (possibilities (matrix-ref board i j)))
    (matrix-set! board i j (car possibilities))
    (try (prune board i j)
         (lambda ()
           (matrix-set! board i j (cdr possibilities))
           (try board fail)))))

;; prune the board with respect to the cell in given position
;; @FIXME: unnecessarily n^2
(define (prune board y x)
  (let ((cell (matrix-ref board y x)))
    (matrix-map-pos
      (lambda (i j)
        (let ((others (matrix-ref board i j)))
          (if (or (not (list? others))
                  (and (= i y) (= j x))
                  (and (not (= i y)) (not (= j x))))
              others
              (filter (lambda (value) (not (= value cell))) others))))
      board)))

;; main program
(let ((solution
  (solve (make-matrix n n (range lo hi))
         possible?
         find-ambiguous
         collapse)))
    (display solution)
    (newline)
    (exit 0))
