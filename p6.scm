#lang racket

(require "base.scm")
(provide part-a part-b)

(define (parse-point s)
  (let ((ps (string-split (s// s "," ""))))
    (point (s->n (first ps)) (s->n (second ps)))))

(define (parse fn)
  (map parse-point (file-lines fn)))

(define (md p0 p1)
  (+ (abs (- (point-x p0) (point-x p1)))
     (abs (- (point-y p0) (point-y p1)))))

(define (closest ps p)
  (let loop ((ps ps) (maxd 100000) (maxp #f))
    (if (null? ps)
      maxp
      (let ((d (md (car ps) p)))
        (cond
          ((= d maxd) (loop (cdr ps) maxd #f))
          ((< d maxd) (loop (cdr ps) d (car ps)))
          (else (loop (cdr ps) maxd maxp)))))))

(define (infinite? ps p)
  (for/or ([i (in-range -5000 5000)])
    (or (equal? p (closest ps (point -5000 i)))
        (equal? p (closest ps (point 5000 i)))
        (equal? p (closest ps (point i -5000)))
        (equal? p (closest ps (point i 5000))))))

(define *map-dim* 2000)

(define (point->idx p)
  (+ (point-x p) (* *map-dim* (point-y p))))

(define (make-imap ps)
  (let ((m (make-vector (* *map-dim* *map-dim*))))
    (for-rect-points (rect -1000 -1000 2000 2000)
      (lambda (p)
        (vector-set! m (point->idx (point+ p (point 1000 1000))) (closest ps p))))
    m))

(define (count-cells im p)
  (length (filter (curry equal? p) (vector->list im))))

(define (part-a fn)
  (let* ((ps (parse fn))
         (im (make-imap ps))
         (cps (filter-not (curry infinite? ps) ps)))
    (count-cells im (argmax (curry count-cells im) cps))))

(define (md-to-all ps p)
  (apply + (map (curry md p) ps)))

(define (count-valid th ps)
  (let ((c 0))
    (for-rect-points (rect -500 -500 1000 1000)
      (lambda (p)
        (if (< (md-to-all ps p) th)
          (set! c (add1 c))
          #f)))
    c))

(define part-b
  (compose (curry count-valid 10000) parse))
