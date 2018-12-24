#lang racket
; p9.scm - AoC 2018 day 9

(require "base.scm")
(provide (all-defined-out))

(struct cvec (val prev next) #:transparent #:mutable)

(define (cvec-make)
  (let ((cv (cvec 0 #f #f)))
    (set-cvec-next! cv cv)
    (set-cvec-prev! cv cv)
    cv))

(define (cvec-fwd cv n)
  (let loop ((cv cv) (n n))
    (if (= 0 n)
      cv
      (loop (cvec-next cv) (sub1 n)))))

(define (cvec-back cv n)
  (let loop ((cv cv) (n n))
    (if (= 0 n)
      cv
      (loop (cvec-prev cv) (sub1 n)))))

(define (cvec-insert cv iv)
  (let* ((np (cvec-fwd cv 1))
         (nn (cvec-fwd cv 2))
         (ncv (cvec iv np nn)))
    (set-cvec-next! np ncv)
    (set-cvec-prev! nn ncv)
    ncv))

(define (cvec-unlink! cv)
  (set-cvec-next! (cvec-prev cv) (cvec-next cv))
  (set-cvec-prev! (cvec-next cv) (cvec-prev cv)))

(define (cvec-remove cv)
  (let ((t (cvec-back cv 7)))
    (cvec-unlink! t)
    (values (cvec-next t)
            (cvec-val t))))

; (0)
; 0 (1)
; 0 (2) 1
; 0 2 1 (3)
; 0 (4) 2 1 3

(define (special-marble? m)
  (= 0 (modulo m 23)))

(define (run-game np nm)
  (let ((scores (make-vector np 0)))
    (let loop ((g (cvec-make)) (turn 0) (marble 1))
      (if (> marble nm)
        scores
        (if (special-marble? marble)
          (let-values (((ng rm) (cvec-remove g)))
            (d->! scores turn (curry + marble rm))
            (loop ng (modulo (add1 turn) np) (add1 marble)))
          (loop (cvec-insert g marble)
                (modulo (add1 turn) np)
                (add1 marble)))))))

(define (parse-str s)
  (let ((ps (string-split s " ")))
    (values
      (s->n (first ps))
      (s->n (seventh ps)))))

(define parse
  (compose parse-str car file-lines))

(define (part-a fn)
  (let-values (((np nm) (parse fn)))
    (vector-argmax
      id
      (run-game np nm))))

(define (part-b fn)
  (let-values (((np nm) (parse fn)))
    (vector-argmax
      id
      (run-game np (* 100 nm)))))
