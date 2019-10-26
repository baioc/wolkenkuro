;;; Miscellaneous utility procedures

;; return list with x as its last element
(define (push list x)
  (append list `(,x)))

;; returns the car of a list if not null, alternatively a default value
(define (maybe-car lst alt)
  (if (null? lst) alt (car lst)))

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

;; return the list ori without any elements in the list rem
(define (purge rem ori)
; (define delete remove) ; Chez/Racket remove === Guile delete
  (if (null? rem) ori
      (purge (cdr rem) (delete (car rem) ori))))
