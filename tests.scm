#!r6rs
;; Test with my as yet unfinished (quickcheck) library, see my github
;; account for the code
(import (quickcheck)
        (rnrs)
        (only (guile) random-state-from-platform)
        (fectors))

(define (prop-update fector i o)
  (-> (< i (fector-length fector))
      (equal? o
              (fector-ref (fector-set fector i o) i))))

(define (prop-update* fector i o)
  (define len (fector-length fector))
  (-> (< i len)
      (let ((new-fector (fector-set fector i o)))
        (for-all (lambda (x)
             (or (= x i)
                 (equal? (fector-ref fector x)
                         (fector-ref new-fector x))))
           (iota len)))))

(define (prop-conversion l)
  (equal? l
          (fector->list (list->fector l))))

(define (prop-length l)
  (equal? (length l)
          (fector-length (list->fector l))))

(define $fector ($=> ($list $integer) (lambda (x) (apply fector x))))

(random-state-from-platform)

(quickcheck prop-update $fector $integer $integer)
(quickcheck prop-update* $fector $integer $integer)
(quickcheck prop-conversion ($list $integer))
(quickcheck prop-length ($list $integer))
