#lang racket
; p1.scm - AoC 2018d1 :)

(require "base.scm")

(provide p1a p1b)

(define (parse fn)
  (map string->number (file-lines fn)))

(define (rsum ins)
  (foldl + 0 ins))

(define p1a (compose rsum parse))

(define (rsumt ins)
  (let loop ((vs (set)) (c 0) (nins ins))
    (let ((n (+ c (car nins))))
      (if (set-member? vs n)
        n
        (loop (set-add vs n) n
          (if (null? (cdr nins))
              ins
              (cdr nins)))))))

(define p1b (compose rsumt parse))
