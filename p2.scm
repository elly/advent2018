#lang racket
; p2.scm - AoC 2018d2

(require "base.scm")

(provide p2a p2b)

(define (lcounts s)
  (let ((cs (make-vector 26 0)))
    (foldl
      (lambda (c v)
        (vector-modify! v (letter->integer c) add1))
      cs (string->list s))))

(define (has2? v) (vector-member 2 v))
(define (has3? v) (vector-member 3 v))

(define parse file-lines)

(define (p2a fn)
  (let ((s (map lcounts (parse fn))))
    (let ((s2 (filter has2? s))
          (s3 (filter has3? s)))
      (* (length s2) (length s3)))))

(define (cdiff s0 s1)
  (let loop ((i 0) (c 0))
    (if (= i (string-length s0))
      c
      (loop (+ i 1)
            (+ c (if (char=? (string-ref s0 i) (string-ref s1 i)) 0 1))))))

(define (f1diff vs)
  (ormap
    (lambda (s0)
      (let ((ss (remove s0 vs)))
        (let ((a (argmin (lambda (s1) (cdiff s0 s1)) ss)))
          (if (= 1 (cdiff s0 a))
            (cons s0 a)
            #f))))
    vs))

(define (comchars ss)
  (let ((s0 (car ss)) (s1 (cdr ss)))
    (let loop ((i 0) (c '()))
      (if (= i (string-length s0))
        (list->string (reverse c))
        (loop (+ i 1)
          (if (char=? (string-ref s0 i) (string-ref s1 i))
            (cons (string-ref s0 i) c)
            c))))))

(define p2b (compose comchars f1diff parse))
