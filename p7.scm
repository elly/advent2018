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

(define (can-start-a? m s)
  (set-empty? (d@ m s (set))))

(define (next-step m p)
  (let ((cs (filter p (allsteps m))))
    (if (null? cs) #f (car (sort cs char<?)))))

(define (finish-step m s)
  (let ((nm (make-hash)))
    (hash-for-each m
      (lambda (k v)
        (let ((nv (e- v s)))
          (hash-set! nm k nv))))
    (hash-remove! nm s)
    nm))

(define (run-a m)
  (let loop ((path '()) (m m))
    (let ((c (next-step m (curry can-start-a? m))))
      (if c
        (loop (cons c path) (finish-step m c))
        (reverse path))))) 

(define (parse fn)
  (build-depmap (map parse-line (file-lines fn))))

(define part-a
  (compose list->string run-a parse))

(define (advance-job j)
  (cons (sub1 (car j)) (cdr j)))

(define (advance-jobs js)
  (filter-not
    (lambda (x) (= (car x) -1))
    (map advance-job js)))

(define (finished-jobs js)
  (filter
    (lambda (x) (= (car x) 0))
    js))

(define (can-start-b? m js s)
  (and 
    (set-empty? (d@ m s (set)))
    (not (memf (lambda (x) (equal? (cdr x) s)) js))))

(define (mark-finished-jobs m js)
  (foldl
    (lambda (j m)
      (if (= (car j) 0)
        (finish-step m (cdr j))
        m))
    m js))

(define (start-more-jobs m js delay nwork)
  (let* ((livejobs (filter-not (lambda (x) (= (car x) 0)) js))
         (n (- nwork (length livejobs))))
    (let loop ((n n) (js js))
      (if (= n 0)
        js
        (let ((j (next-step m (curry can-start-b? m js))))
          (if j
            (loop (sub1 n)
                  (cons (cons (+ delay (char- j #\A)) j) js))
            js))))))

(define (run-b m delay nwork)
  (let loop ((m m) (js '()) (c -1))
    (let* ((nj (advance-jobs js))
           (nm (mark-finished-jobs m nj))
           (nnj (start-more-jobs nm nj delay nwork)))
      (if (null? nnj)
        c
        (loop nm nnj (add1 c))))))

(define (part-b fn)
  (let ((in (parse fn)))
    (run-b in 61 5))) 
