#lang racket
; p8.scm - AoC2018 day 8

(require "base.scm")
(provide (all-defined-out))

(struct tree (metas children) #:transparent)

; parse-tree : tokens -> tree tokens
(define (parse-tree ts)
  (let ((nc (s->n (first ts)))
        (nm (s->n (second ts)))
        (ts (drop ts 2)))
    (let*-values (((children ts) (parse-children nc ts))
                  ((metas ts) (parse-metas nm ts)))
      (values (tree metas children) ts))))

(define (parse-children n ts)
  (let loop ((n n) (cs '()) (ts ts))
    (if (= n 0)
      (values (reverse cs) ts)
      (let-values (((c ts) (parse-tree ts)))
        (loop (sub1 n) (cons c cs) ts)))))

(define (parse-metas n ts)
  (values
    (map s->n (take ts n))
    (drop ts n)))

(define (parse-str s)
  (let-values (((t _) (parse-tree (string-split s " "))))
    t))

(define (parse fn)
  (parse-str (car (file-lines fn))))

(define (sum-metas t)
  (+ (apply + (tree-metas t))
     (apply + (map sum-metas (tree-children t)))))

(define (part-a fn)
  (sum-metas (parse fn)))

(define (valid-meta? t meta)
  (let ((meta (sub1 meta)))
    (and (> (length (tree-children t)) meta)
         (> meta -1))))

(define (tree-value t)
  (if (empty? (tree-children t))
    (apply + (tree-metas t))
    (apply +
      (map
        (lambda (meta)
          (if (valid-meta? t meta)
            (tree-value (list-ref (tree-children t) (sub1 meta)))
            0))
        (tree-metas t)))))

(define (part-b fn)
  (tree-value (parse fn)))
