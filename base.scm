#lang racket

(provide id)
(provide file-lines letter->integer vector-modify!)
(provide point point-x point-y point+ point=?)
(provide rect rect-x rect-y rect-w rect-h for-rect-points)
(provide rect-right rect-bottom rect-bounding)
(provide fix until)
(provide d@ d$ d-> d$! d->!)
(provide e@ e+ e- e+! e-!)
(provide s// srm s->n)
(provide char-)

(struct point (x y) #:transparent)
(struct rect (x y w h) #:transparent)

(define (rect-origin r)
  (point (rect-x r) (rect-y r)))

(define (rect-right r)
  (+ (rect-x r) (rect-w r)))

(define (rect-bottom r)
  (+ (rect-y r) (rect-h r)))

(define (point+ p0 p1)
  (point (+ (point-x p0) (point-x p1))
         (+ (point-y p0) (point-y p1))))

(define (point=? p0 p1)
  (and (= (point-x p0) (point-x p1))
       (= (point-y p0) (point-y p1))))

(define (id x) x)

(define (file-lines fn)
  (let ((f (open-input-file fn)))
    (let loop ((lines '()))
      (let ((l (read-line f)))
        (if (eof-object? l)
          (reverse lines)
          (loop (cons l lines)))))))

(define (letter->integer c)
  (- (char->integer c) (char->integer #\a)))

(define (vector-modify! v i f)
  (let ((r (vector-ref v i)))
    (vector-set! v i (f r))
    v))

(define (for-rect-points r f)
   (let loop ((xi 0) (yi 0) (c 0))
      (cond
        ((= yi (rect-h r)) c)
        ((= xi (rect-w r)) (loop 0 (+ yi 1) c))
        (else
          (begin
            (f (point+ (rect-origin r) (point xi yi)))
            (loop (+ xi 1) yi (+ c 1)))))))

(define (rect-bounding pts)
  (let ((xs (map point-x pts)) (ys (map point-y pts)))
    (rect (apply min xs)
          (apply min ys)
          (add1 (- (apply max xs) (apply min xs)))
          (add1 (- (apply max ys) (apply min ys))))))

(define (fix f in)
  (let loop ((in in) (out (f in)))
    (printf "fix: ~a~n" (vector-length in))
    (if (equal? in out)
      in
      (loop out (f out)))))

(define (until f in)
  (let loop ((in in) (out (f in)))
    (if (not out)
      in
      (loop out (f out)))))

(define d@ dict-ref)
(define d$ dict-set)
(define d-> dict-update)
(define d$! dict-set!)
(define d->! dict-update!)

(define e@ set-member?)
(define e+ set-add)
(define e- set-remove)
(define e+! set-add!)
(define e-! set-remove!)

(define s// string-replace)
(define srm regexp-match)
(define s->n string->number)

(define (char- c0 c1)
  (- (char->integer c0) (char->integer c1)))
