#lang racket

(require "base.scm")
(provide p4a p4b)

(struct dt (year month day hour min) #:transparent)
(struct event (dt id type) #:transparent)
(struct sleep (dt id dur) #:transparent)

(define (dt<? dt0 dt1)
  (let loop ((fs (list dt-year dt-month dt-day dt-hour dt-min)))
    (let ((v0 ((car fs) dt0)) (v1 ((car fs) dt1)))
      (cond
        ((< v0 v1) #t)
        ((> v0 v1) #f)
        ((null? (cdr fs)) #f)
        (else (loop (cdr fs)))))))

(define (dt- dt0 dt1)
  (+ (* 60 (- (dt-hour dt0) (dt-hour dt1)))
     (- (dt-min dt0) (dt-min dt1))))

(define (event<? e0 e1) (dt<? (event-dt e0) (event-dt e1)))

(define *erx-any* #px"\\[(\\d\\d\\d\\d-\\d\\d-\\d\\d \\d\\d:\\d\\d)\\] (.*)")
(define *erx-gid* #px"Guard #(\\d+) begins shift")
(define *erx-sleep* #px"falls asleep")
(define *erx-wake* #px"wakes up")

(define (parse-minsec s)
  (let ((s0 (s// (s// s "-" " ") ":" " ")))
    (apply dt (map s->n (string-split s0)))))

(define (parse-line l)
  (let ((mg (srm *erx-any* l)))
    (let ((t (parse-minsec (second mg))))
      (let ((gp (srm *erx-gid* (third mg)))
            (sp (srm *erx-sleep* (third mg)))
            (wp (srm *erx-wake* (third mg))))
        (cond
          (gp (event t (s->n (second gp)) 'id))
          (sp (event t #f 'sleep))
          (wp (event t #f 'wake)))))))

(define (parse fn)
  (map parse-line (file-lines fn)))

(define (identify events)
  (let ((es (sort events event<?)))
    (let loop ((es es) (gid #f) (nes '()))
      (if (null? es)
        (reverse nes)
        (let ((e (car es)))
          (if
            (symbol=? (event-type e) 'id)
            (loop (cdr es) (event-id e) nes)
            (loop (cdr es) gid
              (cons (event (event-dt e) gid (event-type e)) nes))))))))

(define (flatten1 es ew)
  (let ((d (dt- (event-dt ew) (event-dt es))))
    (sleep (event-dt es) (event-id es) d)))

; list of (gid, sleep, wake) -> list of sleep
(define (flatten events)
  (let loop ((es events) (nes '()))
    (if (null? es)
      (reverse nes)
      (loop (drop es 2) (cons (apply flatten1 (take es 2)) nes)))))

(define (sleepiest sleeps)
  (let ((m (make-hash)))
    (for-each
      (lambda (s)
        (d->! m (sleep-id s)
          (curry + (sleep-dur s))
          0))
      sleeps)
    (car (argmax second (hash-map m list)))))

(define (sleepy-map sleeps gid)
  (let ((m (make-vector 60 0)))
    (for-each
      (lambda (s)
        (if (= (sleep-id s) gid)
          (let loop ((i 0))
            (if (= i (sleep-dur s))
              #f
              (begin
                (let ((ix (+ (dt-min (sleep-dt s)) i)))
                  (d->! m ix add1))
                (loop (+ i 1)))))
            #f))
      sleeps)
    m))

(define (sleepy-minute sleeps gid)
  (let ((m (sleepy-map sleeps gid)))
    (vector-argmax
      (lambda (i) (d@ m i))
      (build-vector 60 (lambda (x) x)))))

(define (p4a fn)
  (let ((es (flatten (identify (parse fn)))))
    (let ((sg (sleepiest es)))
      (let ((sm (sleepy-minute es sg)))
        (* sg sm)))))

(define (gids sleeps)
  (set->list
    (foldl
      (lambda (e ids)
        (e+ ids (sleep-id e)))
      (set) sleeps)))

(define (sleepiest2 sleeps)
  (let ((ids (gids sleeps)))
    (argmax
      (lambda (i)
        (let ((m (sleepy-map sleeps i)))
          (d@ m (sleepy-minute sleeps i))))
      ids)))

(define (p4b fn)
  (let ((es (flatten (identify (parse fn)))))
    (let ((sg (sleepiest2 es)))
      (let ((sm (sleepy-minute es sg)))
        (* sm sg)))))
