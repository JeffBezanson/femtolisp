(defun maplist (f l)
  (if (null l) ()
    (cons (f l) (maplist f (cdr l)))))

; produce a beautiful, toroidal cons structure
; make m copies of a CDR-circular list of length n, and connect corresponding
; conses in CAR-circular loops
; replace maplist 'identity' with 'copy-tree' for rapdily exploding memory use
(defun torus (m n)
  (let* ((l (map-int identity n))
         (g l)
         (prev g))
    (dotimes (i (- m 1))
      (setq prev g)
      (setq g (maplist identity g))
      (rplacd (last prev) prev))
    (rplacd (last g) g)
    (let ((a l)
          (b g))
      (dotimes (i n)
        (rplaca a b)
        (setq a (cdr a))
        (setq b (cdr b))))
    l))

(defun cyl (m n)
  (let* ((l (map-int identity n))
         (g l))
    (dotimes (i (- m 1))
      (setq g (maplist identity g)))
    (let ((a l)
          (b g))
      (dotimes (i n)
        (rplaca a b)
        (setq a (cdr a))
        (setq b (cdr b))))
    l))

(time (progn (print (torus 100 100)) nil))
;(time (dotimes (i 1) (load "100x100.lsp")))
; with ltable
; printing time: 0.415sec
; reading time: 0.165sec

; with ptrhash
; printing time: 0.081sec
; reading time: 0.0264sec
