;;;; GENERIC BACKTRACKING USING CONTINUATION-PASSING STYLE

;; success continuation: (lambda (solution) ...)
;; fail continuation: (lambda () ...)
;; backtracking procedure: (lambda (problem success fail) ...)

;;; given a nondeterministically solvable problem:
;;; if there are still any ambiguities left:
;;;   reduce it in a reversible fashion by commiting to some arbitrary computing
;;;   branch and return the recursive application of this procedure on the
;;;   reduced problem passing in the same success continuation and a new fail
;;;   continuation that undoes the previous reduction and removes that from the
;;;   problem's possibility space before invoking another recursion passing in
;;;   the modified problem and the original continuations;
;;; else, if this path led to an invalid solution:
;;;   call the fail continuation in order to backtrack to a previous state or
;;;   reject the problem as decidedly unsolvable (original fail continuation);
;;; otherwise, the problem has now been collapsed to trivially solvable so just:
;;;   call the success continuation in order to accept that as a solution.

(define (accept solution)
  (display solution) (newline))

(define (reject)
  (display "Impossible!\n"))

(define (backtrack problem success fail)
  ...)

(backtrack ?? accept reject)
