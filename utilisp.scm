;;; Miscellaneous utility procedures

(load "vector.scm") ;; vector-shuffle!


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
    (if (eq? (caar line) 'restriction)
      (cons (car line) (purge-line (cdr line) rem))
      (cons (purge rem (car line)) (purge-line (cdr line) rem))
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
  (if (eq? (car list) 'restriction)
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
  ;(display l)
  ;(newline)
  (if (null? l)
    '()
    (if (pair? (car l))
      (if (eq? (caar l) 'restriction)
        (cons (cons (car l) (split-list-aux (cdr l))) (split-list (cdr l)))
        (split-list (cdr l))
      )
      (split-list (cdr l))
    )))

(define (split-list-aux l)
  (if (null? l)
    '()
    (if (pair? (car l))
      (if (eq? (caar l) 'restriction)
        '()
        (cons (car l) (split-list-aux (cdr l)))
      )
      (cons (car l) (split-list-aux (cdr l)))
    )))

;; make a list with the limitations of quantity of cells and the sum
(define (return-options n sum invert)
  (if (not invert)
    (if (> (- sum (list-ref '(1 3 6 10 15 21 28 36 45) (- n 2))) 9)
      (make-list (+ 1  (- (- sum (list-ref '(1 3 6 10 15 21 28 36 45) (- n 2))) 9) ) 9)
      (make-list 1 (- sum (list-ref '(1 3 6 10 15 21 28 36 45) (- n 2) )))
    )
    (make-list (- sum (list-ref '(9 17 24 30 35 39 42 44 45)(- n 2) )) 9)
    ;(make-list (- sum (list-ref '(9 17 24 30 35 39 42 44 45) (- n 2) )) 9) 
  )
)

;; Make a list starts in n until k
(define (make-list n k)
  (if (> n k)
    '()
    (cons n (make-list (+ n 1) k))
  )
)

(define (correct-val cell val)
  (if (not (list? cell))
    val
    (if (= (length cell) 1)
      cell
      (if (= (length val) 1)
        val
        (make-list (max (car cell) (car val)) (min (list-ref cell (- (length cell) 1)) (list-ref val (- (length val) 1))))
      )
    )
  )
)

