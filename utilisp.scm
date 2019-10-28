;;; Miscellaneous utility procedures

(load "vector.scm") ;; vector-shuffle!

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

;; similar to iota, but as an inclusive [min..max] range
(define (range lo hi)
  (let iter ((k hi) (seq '()))
    (if (< k lo) seq
        (iter (- k 1) (cons k seq)))))

;; gets the car of a list if its not null, alternatively a default value
(define (maybe-car lst alt)
  (if (null? lst) alt (car lst)))

;; return list with x as its last element, like a "reverse cons"
(define (snoc list x)
  (append list `(,x)))

;; return the list ori without any elements in the list rem
(define (purge rem ori)
  ; (define delete remove) ; others' remove <=> Guile's delete
  (if (null? rem) ori
      (purge (cdr rem) (delete (car rem) ori))))

;; remove of ich possibilits the list of values
(define (purge-line line rem)
  (if (null? line)
    '()
    (if (restriction? (car line))
      (cons (car line) (purge-line (cdr line) rem))
      (cons (purge (list rem) (car line)) (purge-line (cdr line) rem))
    )
  )
)

;; make a randomly shuffled list
(define (shuffle list)
  (let ((vec (list->vector list)))
    (vector-shuffle! vec)
    (vector->list vec)))

;; make a randomly shuffled list
(define (shuffle-kakuro list)
  (if (restriction? list)
    list
    (let ((vec (list->vector list)))
      (vector-shuffle! vec)
      (vector->list vec))))


;; return the list ori without any elements in the list rem
(define (purge rem ori)
  ; (define delete remove) ; others' remove <=> Guile's delete
  (if (null? rem) ori
      (purge (cdr rem) (delete (car rem) ori))))

;; split list in cells that are list
(define (split-list l)
  (cond ((null? l) '())
        ((restriction? (car l))
          (cons (cons (car l)
                      (split-list-aux (cdr l)))
                (split-list (cdr l))))
        (else (split-list (cdr l)))))

(define (split-list-aux l)
  (if (or (null? l) (restriction? (car l))) '()
      (cons (car l)
            (split-list-aux (cdr l)))))


;; make a list with the limitations of quantity of cells and the su,
(define (return-options restr n)
  (define ascending '(1 3 6 10 15 21 28 36 45))
  (define descending '(9 17 24 30 35 39 42 44 45))

  (define idx (- n 2))

  (define partial-ascending (list-ref ascending idx))
  (define lower-ascending 1)
  (define higher-ascending (min 9 (- restr partial-ascending)))

  (define partial-descending (list-ref descending idx))
  (define lower-descending (max 1 (- restr partial-descending)))
  (define higher-descending 9)

  (range (max lower-ascending lower-descending)
         (min higher-ascending higher-descending))

  ; (if (not invert)
  ;   (if (> (- restr (list-ref ascending (- n 2))) 9)
  ;     (range 1 9)
  ;     ; (make-list (+ 1  (- (- restr (list-ref ascending (- n 2))) 9) ) 9)
  ;     (range 1 (- restr (list-ref ascending (- n 2) )))
  ;   )
  ;   (if (< (- restr (list-ref descending (- n 2))) 1)
  ;     (range 1 9)
  ;     (range (- restr (list-ref descending (- n 2))) 9)
  ;     ; (make-list (abs (- restr (list-ref descending (- n 2)))) 9)
  ;   )
  ; )
)

(define (correct-val cell val)
  (if (not (list? cell))
    val
    (if (= (length cell) 1)
      cell
      (if (= (length val) 1)
        val
        (range (max (car cell) (car val)) (min (list-ref cell (- (length cell) 1)) (list-ref val (- (length val) 1))))
      )
    )
  )
)
