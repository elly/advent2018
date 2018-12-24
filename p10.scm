#lang racket

(require "base.scm")
(provide (all-defined-out))

(struct elem (pos vec) #:transparent)

(define *point-rx* #px" *(-?\\d+), *(-?\\d+)")
(define (parse-point s)
  (let ((m (srm *point-rx* s)))
    (point (s->n (second m)) (s->n (third m)))))

(define *elem-rx* #px"position=<([^>]+)> velocity=<([^>]+)>")
(define (parse-elem s)
  (let ((m (srm *elem-rx* s)))
    (elem (parse-point (second m)) (parse-point (third m)))))

(define (parse fn)
  (list->vector (map parse-elem (file-lines fn))))

(define (step v)
  (vector-map
    (lambda (e) (elem (point+ (elem-pos e) (elem-vec e)) (elem-vec e)))
    v))

(define (lit? v x y)
  (> (vector-count (lambda (e) (point=? (elem-pos e) (point x y))) v) 0))

(define (bounding v)
  (rect-bounding (vector->list (vector-map elem-pos v))))

(define (pvec v mx my)
  (let ((b (bounding v)))
    (for ((y (rect-h b)))
      (for ((x (rect-w b)))
        (display (if (lit? v (+ x (rect-x b)) (+ y (rect-y b))) #\# #\.)))
      (printf "~n"))))

(define (has-message? v)
  (let ((b (bounding v)))
    (< (rect-h b) 20)))

(define (search v)
  (let loop ((v v) (n 0))
    (if (has-message? v)
      (begin
        (printf "iteration ~a:~n" n)
        (pvec v 80 24))
      (loop (step v) (add1 n)))))

(define part-a (compose search parse))
(define part-b part-a)
