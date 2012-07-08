#!r6rs
(library (union-find)
(export make
        find
        union
        union-find?
        )
(import (except (rnrs) find)
        (fectors))

(define-record-type union-find
  (fields parents ranks))

(define (make n)
  (make-union-find (build-fector n values)
                   (make-fector n 0)))

(define (find uf n)
  (define p (union-find-parents uf))
  (define (parent n)
    (fector-ref p n))
  (define (representative? n)
    (= n (fector-ref p n)))
  (define (path-compress! n rep)
    (union-find-parents-set! uf (fector-set p n rep)))
  (define (search n)
    (if (representative? n)
        n
        (search (parent n))))
  (let ((representative (search n)))
    (path-compress! n representative)
    representative))

(define (union uf n1 n2)
  (define ranks (union-find-ranks uf))
  (define (rank i)
    (fector-ref ranks i))
  (define (increment-rank i)
    (fector-set ranks i (+ 1 (rank i))))

  (define parents (union-find-parents uf))
  (define (set-parent a b)
    (fector-set parents a b))
  
  (define rep-for-1 (find uf n1))
  (define rep-for-2 (find uf n2))
  (if (= rep-for-1 rep-for-2)
      uf
      (let ((rank-for-1 (rank rep-for-1))
            (rank-for-2 (rank rep-for-2)))
        (cond ((< rank-for-1 rank-for-2)
               (make-union-find (set-parent rep-for-1 rep-for-2)
                                ranks))
              ((< rank-for-2 rank-for-1)
               (make-union-find (set-parent rep-for-2 rep-for-1)
                                ranks))
              (else
               (make-union-find (set-parent rep-for-2 rep-for-1)
                                (increment-rank rep-for-1)))))))

)
