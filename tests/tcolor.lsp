; -*- scheme -*-
; color for performance

(load "color.lsp")

; 100x color 5 queens
(define Q (generate-5x5-pairs))
(define (ct)
  (set! C (color-pairs Q '(a b c d e)))
  (dotimes (n 99) (color-pairs Q '(a b c d e))))
(time (ct))
(assert (equal? C
		'((23 . a) (9 . a) (22 . b) (17 . d) (14 . d) (8 . b) (21 . e)
		  (19 . b) (16 . c) (13 . c) (11 . b) (7 . e) (24 . c) (20 . d)
		  (18 . e) (15 . a) (12 . a) (10 . e) (6 . d) (5 . c) (4 . e)
		  (3 . d) (2 . c) (0 . b) (1 . a))))
