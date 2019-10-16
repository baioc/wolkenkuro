(define (make-restriction col row)
  (list 'restriction col row))

(define (restriction-column restr)
  (cadr restr))

(define (restriction-row restr)
  (caddr restr))

(define (make-kakuro n)
  (define r make-restriction)
  (cond ((= n 0)
          `((,(r 0 0)  ,(r 27 0) ,(r 15 0) ,(r 13 0) ,(r 35 0) ,(r 0 0))
            (,(r 0 28) 0         0         0         0         ,(r 12 0))
            (,(r 0 16) 0         0         0         0         0)
            (,(r 0 14) 0         0         ,(r 9 8)  0         0)
            (,(r 0 19) 0         0         0         0         0)
            (,(r 0 0)  ,(r 0 16) 0         0         0         0)))
        ))
