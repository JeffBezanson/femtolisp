; -*- scheme -*-
(define (maplist f l)
  (if (null? l) ()
    (cons (f l) (maplist f (cdr l)))))

; produce a beautiful, toroidal cons structure
; make m copies of a CDR-circular list of length n, and connect corresponding
; conses in CAR-circular loops
; replace maplist 'identity' with 'copy-tree' for rapdily exploding memory use
(define (torus m n)
  (let* ((l (map-int identity n))
         (g l)
         (prev g))
    (dotimes (i (- m 1))
      (set! prev g)
      (set! g (maplist identity g))
      (set-cdr! (last-pair prev) prev))
    (set-cdr! (last-pair g) g)
    (let ((a l)
          (b g))
      (dotimes (i n)
        (set-car! a b)
        (set! a (cdr a))
        (set! b (cdr b))))
    l))

(define (cyl m n)
  (let* ((l (map-int identity n))
         (g l))
    (dotimes (i (- m 1))
      (set! g (maplist identity g)))
    (let ((a l)
          (b g))
      (dotimes (i n)
        (set-car! a b)
        (set! a (cdr a))
        (set! b (cdr b))))
    l))

(time (begin (print (torus 100 100)) ()))
;(time (dotimes (i 1) (load "100x100.lsp")))
; with ltable
; printing time: 0.415sec
; reading time: 0.165sec

; with ptrhash
; printing time: 0.081sec
; reading time: 0.0264sec
