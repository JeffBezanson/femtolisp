expand
append
bq-process

(define (syntax-environment)
  (map (lambda (s) (cons s (symbol-syntax s)))
       (filter symbol-syntax (environment))))

(syntax-environment)

(symbol-syntax 'try)

(map-int (lambda (x) `(a b c d e)) 90)

(list->vector (map-int (lambda (x) `(a b c d e)) 90))

'((lambda (x y) (if (< x y) x y)) (a b c) (d e f) 2 3 (r t y))

'((lambda (x y) (if (< x y) x yffffffffffffffffffff)) (a b c) (d e f) 2 3 (r t y))

'((lambda (x y) (if (< x y) x y)) (a b c) (d (e zz zzz) f) 2 3 (r t y))

'((23 . a) (9 . a) (22 . b) (17 . d) (14 . d) (8 . b) (21 . e)
  (19 . b) (16 . c) (13 . c) (11 . b) (7 . e) (24 . c) (20 . d)
  (18 . e) (15 . a) (12 . a) (10 . e) (6 . d) (5 . c) (4 . e)
  (3 . d) (2 . c) (0 . b) (1 . a))
