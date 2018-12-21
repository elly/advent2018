#lang racket
; p5.scm - aoc2018d5

(require "base.scm")
(provide (all-defined-out))

(define (parse fn)
  (list->vector (string->list (car (file-lines fn)))))

(define (react? c0 c1)
  (and (char=? (char-downcase c0) (char-downcase c1))
       (not (char=? c0 c1))))

(define (target v)
  (for/first ([i (sub1 (vector-length v))]
                 #:when (react? (vector-ref v i) (vector-ref v (add1 i))))
    i))

(define (react-left v)
  (let ((t (target v)))
    (if t
      (begin
        (vector-copy! v t v (+ 2 t))
        (vector-drop-right v 2))
      #f)))

(define (react v)
  (let loop ((v v))
    (let ((r (react-left v)))
      (if r (loop r) v))))

(define part-a (compose vector-length react parse))

(define (react-without v k)
  (react (vector-filter-not
    (lambda (c) (char=? (char-downcase c) k)) v)))

(define (best-letter v)
  (argmin
    (lambda (c)
      (vector-length (react-without v c)))
    (string->list "abcdefghijklmnopqrstuvwxyz")))

(define (part-b fn)
  (let ((input (parse fn)))
    (vector-length (react-without input (best-letter input)))))
