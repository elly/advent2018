#lang racket

(require "base.scm")

(provide (all-defined-out))

(define *line-rx* #px"Step (.) must be finished before step (.) .*")
(define (parse-line s)
  (let ((m (srm *line-rx* s)))
    (cons (string-ref (second m) 0) (string-ref (third m) 0))))

(define (build-depmap ps)
  (let ((m (make-hash)))
    (for-each
      (lambda (p)
        (d->! m (cdr p)
          (lambda (s) (e+ s (car p)))
          (set)))
      ps)
    m))

(define (allsteps m)
  (set->list (set-union
    (list->set (hash-keys m))
    (list->set (apply set-union (set) (hash-values m))))))

(define (can-start? m s)
  (set-empty? (d@ m s (set))))

(define (next-step m)
  (let ((cs (filter (curry can-start? m) (allsteps m))))
    (if (null? cs) #f (car (sort cs char<?)))))

(define (finish-step m s)
  (let ((nm (make-hash)))
    (hash-for-each m
      (lambda (k v)
        (let ((nv (e- v s)))
          (hash-set! nm k nv))))
    (hash-remove! nm s)
    (printf "~a~n" nm)
    nm))

(define (run m)
  (let loop ((path '()) (m m))
    (let ((c (next-step m)))
      (if c
        (loop (cons c path) (finish-step m c))
        (reverse path))))) 

(define (parse fn)
  (build-depmap (map parse-line (file-lines fn))))

(define part-a
  (compose list->string run parse))
