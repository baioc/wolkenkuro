;;;; GENERIC BACKTRACKING USING CONTINUATION-PASSING STYLE

;; success (return) continuation: (lambda (solution) ...)
;; fail (backtrack/reject) continuation: (lambda () ...)
;; elimination (collapse) procedure:
;;   (lambda (try/else problem fail)
;;     (try/else something
;;               (lambda () (try/else otherthing fail))))

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

(define (tree-prune try root fail)
  ;; try going left first
  (display "trying left... ") (display (car root)) (newline)
  (try (car root)
       (lambda ()
         ;; when backtracking, go right instead
         (display "trying right... ") (display (cadr root)) (newline)
         (try (cadr root) fail))))

(define (tree-find node)
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
