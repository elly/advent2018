#lang racket

(require "base.scm")

(provide p3a p3b)

(struct claim (id rect))

(define (parse-claim s)
  (let ((m (regexp-match #px"#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)" s))
        (s->i string->number))
    (claim (s->i (second m))
           (rect (s->i (third m))
                 (s->i (fourth m))
                 (s->i (fifth m))
                 (s->i (sixth m))))))

(define (parse fn)
  (map parse-claim (file-lines fn)))

(define (claim-cells c m)
  (for-rect-points (claim-rect c)
    (lambda (p)
      (hash-update! m p add1 0)))
  m)

(define (cmap claims)
  (let ((m (make-hash)))
    (foldl claim-cells m claims)))

(define (filter-vals m p)
  (filter p (hash-values m)))

(define (vals>1 m)
  (length (filter-vals m (lambda (x) (> x 1)))))

(define p3a
  (compose vals>1 cmap parse))

(define (unoverlapped? m c)
  (let ((good #t))
    (for-rect-points (claim-rect c)
      (lambda (p)
        (if (> (hash-ref m p) 1)
          (set! good #f) #f)))
    (if good c #f)))

(define (unoverlapped-claim m cs)
  (ormap
    (lambda (c) (unoverlapped? m c))
    cs))

(define (p3b fn)
  (let ((cs (parse fn)))
    (claim-id (unoverlapped-claim (cmap cs) cs))))
