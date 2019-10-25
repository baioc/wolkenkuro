;;;; GENERIC BACKTRACKING USING CONTINUATION-PASSING STYLE

;; success continuation: (lambda (solution) ...)
;; fail continuation: (lambda () ...)
;; backtracking procedure: (lambda (problem success fail) ...)

;;; given a nondeterministically solvable problem:
;;; if there are still any ambiguities left to resolve:
;;;   reduce it in a reversible fashion by commiting to some arbitrary computing
;;;   branch and return the recursive application of this procedure on the
;;;   reduced problem passing in the same success continuation and a new fail
;;;   continuation that undoes the previous reduction and removes that from the
;;;   problem's solution possibility space before invoking another recursion
;;;   passing in the modified problem and both the original continuations;
;;; otherwise, if this path led to an invalid solution:
;;;   call the fail continuation in order to backtrack to a previous state or
;;;   reject the problem as decidedly unsolvable (original fail continuation);
;;; else, the problem has now collapsed to trivially solvable so just:
;;;   call the success continuation in order to accept that as a solution.

(define (solve solutions succeed fail)
  (cond ((amb? solutions)
          (collapse solutions
                    fail
                    (lambda (reduced backtrack)
                      (solve reduced succeed backtrack))))
        ((not (valid? solutions)) (fail))
        (else (succeed solutions))))


;;; example

(define (accept solution)
  (display "Found it -> ") (display solution) (newline))

(define (reject)
  (display "Impossible!\n"))

(define (amb? solution)
  (list? solution))

(define (valid? solution)
  (eq? solution 'RLR))

(define (collapse problem fail try)
  (display "trying left... ") (display (car problem)) (newline)
  (try (car problem)
       (lambda ()
         (display "trying right... ") (display (cadr problem)) (newline)
         (try (cadr problem) fail))))

(define problem ; decision-tree
  '(
    (
      (
        LLL
        LLR
      )
      (
        LRL
        LRR
      )
    )
    (
      (
        RLL
        RLR
      )
      (
        RRL
        RRR
      )
    )
  )
)

(display "Begin solving ") (display problem) (newline)
(solve problem accept reject)
