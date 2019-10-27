(load "backtrack.scm")


;;; a simple example

(define binary-labyrinth '(((LLL LLR) (LRL LRR)) ((RLL RLR) (RRL RRR))))

(define (dfs tree node)
  (define (tree-prune try root fail _)
    (let ((left (car root)) (right (cadr root)))
      (display "found a bifurcation: ") (display root) (newline)
      (display "trying left...\n")
      (try left
           (lambda ()
             (display "oops, wrong way! back to ") (display root) (newline)
             (display "trying right...\n")
             (try right fail)))))
  (solve tree                  ;; original problem
         list?                 ;; check if there's still work to be done
         (lambda (possibility) ;; check if a solution was found
           (equal? possibility node))
         tree-prune))          ;; what to do at each decision point

; (dfs binary-labyrinth 'RLR)


;;; a more robust Proof Of Concept with the n-Queens puzzle

(load "vector.scm") ;; vector-find-pos, vector-every-pos, vector-map-pos,
                    ;; vector-map
(load "utilisp.scm") ;; shuffle

(define (queens n)
  ;; check if positions for two queens lead them to a check
  (define (in-check? xa ya xb yb)
    (or (= xa xb) ; same row
        (= ya yb) ; same col
        (= (abs (- xa xb)) (abs (- ya yb))))) ; same diag

  ;; vector containing each of the n queens' possible positions
  (define (make-queens n)
    (define (board i j lst)
      (cond ((= i 0) lst)
            ((= j 0) (board (- i 1) n lst))
            (else (board i (- j 1) (cons (cons i j) lst)))))
    (vector-map shuffle (make-vector n (board n n '()))))

  ;; check if there's any ambiguity left to resolve, return its position
  (define (amb? board)
    (vector-find-pos
      (lambda (q)
        (let ((queen (vector-ref board q)))
          (and (list? queen) (not (null? queen)))))
      board))

  ;; check if the board is a solution to the queens puzzle
  (define (valid? positions)
    (define (safe? q)
      (let ((queen (vector-ref positions q)))
        (if (null? queen) #f
            (let ((qx (car queen)) (qy (cdr queen)))
              (vector-every-pos
                (lambda (k)
                  (let ((other (vector-ref positions k)))
                    (if (null? other) #f
                        (or (= k q)
                            (not (in-check? qx qy (car other) (cdr other)))))))
                positions)))))
    (vector-every-pos safe? positions))

  ;; make a guess by fixing some queen's position
  (define (emplace try possibilities fail position)
    (let ((queen (vector-ref possibilities position)))
      (vector-set! possibilities position (car queen))
      (try (prune possibilities position) ;; prune others' possibilities
           (lambda ()
             ;; restore original possibilities except for the one from this try
             (vector-set! possibilities position (cdr queen))
             (try possibilities fail)))))

  ;; prune possibility space with respect to q-positioned queen
  (define (prune board q)
    (let* ((queen (vector-ref board q))
           (x (car queen)) (y (cdr queen)))
      (vector-map-pos
        (lambda (k)
          (let ((others (vector-ref board k)))
            (if (or (= k q) (not (list? others))) others
                (filter (lambda (position)
                          (not (in-check? x y (car position) (cdr position))))
                        others))))
        board)))

  (solve (make-queens n)
          amb?
          valid?
          emplace))

; (display (queens 8))
