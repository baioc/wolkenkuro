;;;; GENERIC BACKTRACKING USING CONTINUATION-PASSING STYLE

;; success continuation:
;;   (lambda (solution) <...>)
;;
;; fail continuation:
;;   (lambda () <...>)
;;
;; pruning procedure:
;;   (lambda (try/else problem fail tip)
;;     <do-something-with-tip>
;;     (try/else <reduced-problem>
;;               (lambda ()
;;                 <undo-that-something>
;;                 (try/else <otherthings> fail))))

;;; given a nondeterministically solvable problem:
;;; check whether this path led to an invalid solution and if so:
;;;   call the current fail continuation so as to either backtrack to a previous
;;;   state and retry from there or reject the problem as decidedly unsolvable;
;;; if there are still any ambiguities left to resolve:
;;;   reduce it in a reversible fashion by commiting to some possibly arbitrary
;;;   computing branch and return the recursive application of this procedure
;;;   passing in the reduced problem and a new fail continuation that:
;;;     undoes the previous reduction and removes that from the solution
;;;     possibility space before recursing yet again - that is, keep trying -
;;;     with the newly modified problem and the original fail continuation;
;;; else, the problem has now collapsed to trivially solvable so just:
;;;   call the success continuation in order to accept that solution.

(define (solve problem possible? ambiguous? collapse)
  (call/cc (lambda (return)
    (let retry ((solutions problem)
                (backtrack (lambda () (return #f))))
      (cond ((not (possible? solutions)) (backtrack))
            ((ambiguous? solutions) =>
               (lambda (ambiguity)
                 (collapse retry solutions backtrack ambiguity)))
            (else (return solutions)))))))

;;; see test.scm for example usage
