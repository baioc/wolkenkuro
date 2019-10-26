;; perform the Fisher-Yates-Knuth shuffle on a list
(define (shuffle list)
  (let ((n (length list))
        (vec (list->vector list)))
    (let loop ((i (- n 1)))
      (if (> i 0)
          (let ((j (random (+ i 1))))
            (if (not (= i j))
              (let ((x (vector-ref vec i)))
                (vector-set! vec i (vector-ref vec j))
                (vector-set! vec j x)))
            (loop (- i 1)))))
    (vector->list vec)))

;; identity function
(define (identity x) x)

;; checks if there exists any element in the list that satisfies a predicate
(define (any pred list)
  (cond ((null? list) #f)
        ((pred (car list)) => identity)
        (else (any pred (cdr list)))))

;; checks if an element x satisfies a predicate for every x in the list
(define (every pred list)
  (not (any (lambda (x) (not (pred x))) list)))
