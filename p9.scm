#lang racket
; p9.scm - AoC 2018 day 9

(require "base.scm")
(provide (all-defined-out))

(struct cvec (vec index) #:transparent)

(define (cvec-insert cv iv)
  (let* ((i (modulo (+ (cvec-index cv) 2) (length (cvec-vec cv))))
         (left (take (cvec-vec cv) i))
         (right (drop (cvec-vec cv) i)))
    (cvec (append left (list iv) right) i)))

(define (cvec-remove cv)
  (let* ((i (modulo (- (cvec-index cv) 7) (length (cvec-vec cv))))
         (left (take (cvec-vec cv) i))
         (right (drop (cvec-vec cv) (+ i 1))))
    (values (cvec (append left right) i)
            (list-ref (cvec-vec cv) i))))

; (0)
; 0 (1)
; 0 (2) 1
; 0 2 1 (3)
; 0 (4) 2 1 3

(define (special-marble? m)
  (= 0 (modulo m 23)))

(define (run-game np nm)
  (let ((scores (make-vector np 0)))
    (let loop ((g (cvec '(0) 0)) (turn 0) (marble 1))
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

(define (try-some-stuff)
  (for ((i 50))
    (printf "~a: ~a~n" i (vector-argmax id (run-game 9 (* 23 i))))))
