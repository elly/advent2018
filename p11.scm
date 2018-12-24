#lang racket
; AoC 2018 day 11 part 1

(require "base.scm")
(provide (all-defined-out))

(define (hundreds n)
  (modulo (floor (/ n 100)) 10))

(define (power-at ser pt)
  (let ((rid (+ (point-x pt) 10)))
    (-
      (hundreds
        (* rid (+ ser (* rid (point-y pt)))))
      5)))

(define *grid-dim* 300)
(define (grid-idx pt)
  (+ (* *grid-dim* (point-y pt)) (point-x pt)))
(define (grid-unidx idx)
  (point (modulo idx *grid-dim*) (floor (/ idx *grid-dim*))))

(define (build-grid ser)
  (build-vector (* *grid-dim* *grid-dim*)
    (lambda (i)
      (power-at ser (grid-unidx i)))))

(define (g@ g x y) (d@ g (grid-idx (point x y))))

(define (score-3x3 g p)
  (let ((x (point-x p)) (y (point-y p)))
    (+ (g@ g x y)       (g@ g (+ 1 x) y)       (g@ g (+ 2 x) y)
       (g@ g x (+ 1 y)) (g@ g (+ 1 x) (+ 1 y)) (g@ g (+ 2 x) (+ 1 y))
       (g@ g x (+ 2 y)) (g@ g (+ 1 x) (+ 2 y)) (g@ g (+ 2 x) (+ 2 y)))))

(define (largest-3x3 gv)
  (let ((mp #f) (ms 0))
    (for ((y (- *grid-dim* 2)))
      (for ((x (- *grid-dim* 2)))
        (let ((s (score-3x3 gv (point x y))))
          (if (> s ms)
            (begin
              (set! mp (point x y))
              (set! ms s))
            #f))))
    (cons mp ms)))

(define part-a
  (compose largest-3x3 build-grid))
