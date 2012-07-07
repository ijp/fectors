#!r6rs
;;; fectors.sls --- Functional Vectors

;; Copyright (C) 2012 Ian Price <ianprice90@googlemail.com>

;; Author: Ian Price <ianprice90@googlemail.com>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

(library (fectors)
(export make-fector
        fector
        fector?
        fector-length
        build-fector
        fector-ref
        fector-set
        list->fector
        fector->list
        )
(import (rnrs))

;;; Utilities
(define (build-vector n f)
  (assert (>= n 0))
  (let ((v (make-vector n)))
    (define (populate! i)
      (unless (= i n)
        (vector-set! v i (f i))
        (populate! (+ i 1))))
    (populate! 0)))

;;; Main
(define-record-type (<fector> %make-fector fector?)
  (fields
   (mutable value fector-value fector-value-set!)))

(define-record-type diff
  (fields (immutable index)
          (mutable value)
          (mutable parent)))

(define make-fector
  (case-lambda
    ((n) (%make-fector (make-vector n)))
    ((n fill) (%make-fector (make-vector n fill)))))

(define (fector . values)
  (%make-fector (apply vector values)))

(define (build-fector n f)
  (%make-fector (build-vector n f)))


(define (fector-length fector)
  (reroot! fector)
  (vector-length (fector-value fector)))

(define (fector-ref fector index)
  (reroot! fector)
  (vector-ref (fector-value fector) index))

(define (fector-set fector index object)
  (reroot! fector)
  (let ((v (fector-value fector)))
    (assert (and (<= 0 index)
                 (< index (vector-length v))))
    (let ((old-value (vector-ref v index)))
      (vector-set! v index object)
      (let* ((new-fector (%make-fector v))
             (diff (make-diff index old-value new-fector)))
        (fector-value-set! fector diff)
        new-fector))))

(define (reroot! fector)
  (define value (fector-value fector))
  (if (diff? value)
      (let* ((index (diff-index value))
             (parent (reroot! (diff-parent value)))
             (parent-vector  (fector-value parent))
             ;; currently make new diff -- should reuse existing
             (diff (make-diff index (vector-ref parent-vector index) fector)))
        (vector-set! parent-vector index (diff-value value))
        (fector-value-set! fector parent-vector)
        (fector-value-set! parent diff)
        fector)
      fector))

(define (list->fector l)
  (%make-fector (list->vector l)))

(define (fector->list fector)
  (reroot! fector)
  (vector->list (fector-value fector)))

)
