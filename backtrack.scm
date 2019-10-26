;;;; GENERIC BACKTRACKING USING CONTINUATION-PASSING STYLE

;; success (return) continuation: (lambda (solution) ...)
;; fail (backtrack/reject) continuation: (lambda () ...)
;; elimination (collapse) procedure:
;; (lambda (try/else problem fail)
;;   (try/else something
;;             (lambda () (try/else otherthings fail))))

;;; given a nondeterministically solvable problem:
;;; if there are still any ambiguities left to resolve:
;;;   reduce it in a reversible fashion by commiting to some possibly arbitrary
;;;   computing branch and return the recursive application of this procedure
;;;   passing in <the reduced problem> and <a new fail continuation> that:
;;;     undoes the previous reduction and removes that from the solution
;;;     possibility space before recursing yet again with the newly modified
;;;     problem and the original fail continuation;
;;; otherwise, check whether this path led to an invalid solution and if so:
;;;   call the current fail continuation so as to either backtrack to a previous
;;;   state and retry from there or reject the problem as decidedly unsolvable;
;;; else, the problem has now collapsed to trivially solvable so just:
;;;   call the success continuation in order to accept that solution.

(define (solve problem return fail ambiguous? valid? collapse)
  (let retry ((solutions problem) (backtrack fail))
    (cond ((ambiguous? solutions)
            (collapse (lambda (reduced undo) (retry reduced undo))
                      solutions
                      backtrack))
          ((not (valid? solutions)) (backtrack))
          (else (return solutions)))))


;;; a simple example

(define search-tree '(((LLL LLR) (LRL LRR)) ((RLL RLR) (RRL RRR))))

(define (tree-find node)
  (define (tree-prune try root fail)
    ;; try going left first
    (display "trying left... ") (display (car root)) (newline)
    (try (car root)
         (lambda ()
           ;; when backtracking, go right instead
           (display "trying right... ") (display (cadr root)) (newline)
           (try (cadr root) fail))))
  (display "Begin solving ") (display search-tree) (newline)
  (solve search-tree           ;; original problem
         (lambda (solution)    ;; success continuation
           (display "Found it -> ") (display solution) (newline))
         (lambda ()            ;; fail continuation
           (display "Impossible!"))
         (lambda (maybe)       ;; test if there's still work to be done
           (list? maybe))
         (lambda (possibility) ;; test if a solution was found
           (eq? possibility node))
         tree-prune)           ;; what to do at each decision point
)

; (tree-find 'RLR)


;;; Proof Of Concept with the Eight Queens puzzle

(load "common.scm") ;; shuffle

(define (eight-queens)
  ;; vector containing each of the n queens' possible positions
  (define (queens n)
    (define (board i j lst)
      (cond ((= i 0) lst)
            ((= j 0) (board (- i 1) n lst))
            (else (board i (- j 1) (cons (cons i j) lst)))))
    (vector-map shuffle (make-vector n (board n n '()))))

  ;; finds the first index of a vector that satisfies a predicate, else #f
  (define (vector-find pred vec)
    (let for ((k 0) (n (vector-length vec)))
      (cond ((= k n) #f)
            ((pred k) k)
            (else (for (+ k 1) n)))))

  ;; checks if a vector index satisfies a predicate for every valid index
  (define (vector-every pred vec)
    (not (vector-find (lambda (idx) (not (pred idx))) vec)))

  ;; check if there are still any ambiguities left to resolve
  (define (amb? board)
    (vector-find (lambda (q)
                   (let ((queen (vector-ref board q)))
                     (and (list? queen) (not (null? queen)))))
                 board))

  ;; check if the board is a solution to the queens puzzle
  (define (valid? positions)
    (define (safe? q)
      (let ((queen (vector-ref positions q)))
        (if (null? queen) #f
            (let ((qx (car queen)) (qy (cdr queen)))
              (vector-every (lambda (k)
                              (let ((other (vector-ref positions k)))
                                (if (null? other) #f
                                    (or (= k q)
                                      (let ((x (car other)) (y (cdr other)))
                                        (and (not (= x qx))
                                             (not (= y qy))
                                             (not (= (abs (- x qx))
                                                     (abs (- y qy))))))))))
                            positions)))))
    (vector-every safe? positions))

  ;; make a guess by collapsing some queen's position
  (define (emplace try possibilities fail)
    ;; possibility space pruning
    (define (prune board qx qy)
      (vector-map (lambda (others)
                    (if (list? others)
                        (filter (lambda (position)
                                  (let ((x (car position)) (y (cdr position)))
                                    (and (not (= x qx))
                                         (not (= y qy))
                                         (not (= (abs (- x qx))
                                                 (abs (- y qy)))))))
                                others)
                        others))
                  board))
    (let* ((q (amb? possibilities))
           (queens (vector-ref possibilities q))
           (queen (car queens))
           (qx (car queen)) (qy (cdr queen)))
      (vector-set! possibilities q queen)
      (try (prune possibilities qx qy)
           (lambda ()
             ;; restore (almost) original possibilities
             (vector-set! possibilities q (cdr queens))
             (try possibilities fail)))))

  (call-with-current-continuation (lambda (return)
    (solve (queens 8)
           return
           (lambda () #f)
           amb?
           valid?
           emplace))))

; (display (eight-queens))
