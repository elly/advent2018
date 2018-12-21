#lang racket

(provide file-lines letter->integer vector-modify!)
(provide point point-x point-y)
(provide rect rect-x rect-y rect-w rect-h for-rect-points)

(struct point (x y) #:transparent)
(struct rect (x y w h) #:transparent)

(define (rect-origin r)
  (point (rect-x r) (rect-y r)))

(define (point+ p0 p1)
  (point (+ (point-x p0) (point-x p1))
         (+ (point-y p0) (point-y p1))))

(define (point=? p0 p1)
  (and (= (point-x p0) (point-x p1))
       (= (point-y p0) (point-y p1))))

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
