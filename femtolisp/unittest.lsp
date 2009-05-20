; -*- scheme -*-
(define (every-int n)
  (list (fixnum n) (int8 n) (uint8 n) (int16 n) (uint16 n) (int32 n) (uint32 n)
        (int64 n) (uint64 n)))

(define (every-sint n)
  (list (fixnum n) (int8 n) (int16 n) (int32 n) (int64 n)))

(define (each f l)
  (if (atom? l) ()
      (begin (f (car l))
	     (each f (cdr l)))))

(define (each^2 f l m)
  (each (lambda (o) (each (lambda (p) (f o p)) m)) l))

(define (test-lt a b)
  (each^2 (lambda (neg pos)
            (begin
              (eval `(assert (= -1 (compare ,neg ,pos))))
              (eval `(assert (=  1 (compare ,pos ,neg))))))
          a
          b))

(define (test-eq a b)
  (each^2 (lambda (a b)
            (begin
              (eval `(assert (= 0 (compare ,a ,b))))))
          a
          b))

(test-lt (every-sint -1) (every-int 1))
(test-lt (every-int 0) (every-int 1))
(test-eq (every-int 88) (every-int 88))
(test-eq (every-sint -88) (every-sint -88))

(define (test-square a)
  (each (lambda (i) (eval `(assert (>= (* ,i ,i) 0))))
        a))

(test-square (every-sint -67))
(test-square (every-int 3))
(test-square (every-int 0x80000000))
(test-square (every-sint 0x80000000))
(test-square (every-sint -0x80000000))

(assert (= (* 128 0x02000001) 0x100000080))

(assert (= (/ 1) 1))
(assert (= (/ -1) -1))
(assert (= (/ 2.0) 0.5))

(assert (= (- 4999950000 4999941999) 8001))

; tricky cases involving INT_MIN
(assert (< (- #uint32(0x80000000)) 0))
(assert (> (- #int32(0x80000000)) 0))
(assert (< (- #uint64(0x8000000000000000)) 0))
(assert (> (- #int64(0x8000000000000000)) 0))

(assert (not (equal #int64(0x8000000000000000) #uint64(0x8000000000000000))))
(assert (equal (+ #int64(0x4000000000000000) #int64(0x4000000000000000))
               #uint64(0x8000000000000000)))
(assert (equal (* 2 #int64(0x4000000000000000))
               #uint64(0x8000000000000000)))

(assert (equal (uint64 (double -123)) #uint64(0xffffffffffffff85)))

(assert (equal (string 'sym #byte(65) #wchar(945) "blah") "symA\u03B1blah"))

; NaNs
(assert (equal? +nan.0 +nan.0))
(assert (not (= +nan.0 +nan.0)))
(assert (not (= +nan.0 -nan.0)))
(assert (equal? (< +nan.0 3) (> 3 +nan.0)))
(assert (equal? (< +nan.0 (double 3)) (> (double 3) +nan.0)))
(assert (equal? (< +nan.0 3) (> (double 3) +nan.0)))
(assert (equal? (< +nan.0 (double 3)) (> 3 +nan.0)))
(assert (equal? (< +nan.0 3) (< +nan.0 (double 3))))
(assert (equal? (> +nan.0 3) (> +nan.0 (double 3))))
(assert (equal? (< 3 +nan.0) (> +nan.0 (double 3))))
(assert (equal? (> 3 +nan.0) (> (double 3) +nan.0)))
(assert (not (>= +nan.0 +nan.0)))

; -0.0 etc.
(assert (not (equal? 0.0 0)))
(assert (equal? 0.0 0.0))
(assert (not (equal? -0.0 0.0)))
(assert (not (equal? -0.0 0)))
(assert (not (eqv? 0.0 0)))
(assert (not (eqv? -0.0 0)))
(assert (not (eqv? -0.0 0.0)))
(assert (= 0.0 -0.0))

; this crashed once
(for 1 10 (lambda (i) 0))

; long argument lists
(assert (= (apply + (iota 100000)) 4999950000))

; ok, a couple end-to-end tests as well
(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
(assert (equal (fib 20) 6765))

(load "color.lsp")
(assert (equal (color-pairs (generate-5x5-pairs) '(a b c d e))
               '((23 . a) (9 . a) (22 . b) (17 . d) (14 . d) (8 . b) (21 . e)
                 (19 . b) (16 . c) (13 . c) (11 . b) (7 . e) (24 . c) (20 . d)
                 (18 . e) (15 . a) (12 . a) (10 . e) (6 . d) (5 . c) (4 . e)
                 (3 . d) (2 . c) (0 . b) (1 . a))))

; hashing strange things
(assert (equal?
	 (hash '#0=(1 1 #0# . #0#))
	 (hash '#1=(1 1 #1# 1 1 #1# . #1#))))

(assert (not (equal?
	      (hash '#0=(1 1 #0# . #0#))
	      (hash '#1=(1 2 #1# 1 1 #1# . #1#)))))

(assert (equal?
	 (hash '#0=((1 . #0#) . #0#))
	 (hash '#1=((1 . #1#) (1 . #1#) . #1#))))

(assert (not (equal?
	      (hash '#0=((1 . #0#) . #0#))
	      (hash '#1=((2 . #1#) (1 . #1#) . #1#)))))

(assert (not (equal?
	      (hash '#0=((1 . #0#) . #0#))
	      (hash '#1=((1 . #1#) (2 . #1#) . #1#)))))

(assert (equal?
	 (hash #0=[1 [2 [#0#]] 3])
	 (hash #1=[1 [2 [[1 [2 [#1#]] 3]]] 3])))

(assert (not (equal?
	      (hash #0=[1 [2 [#0#]] 3])
	      (hash #1=[1 [2 [[5 [2 [#1#]] 3]]] 3]))))

(assert (equal?
	 (hash #0=[1 #0# [2 [#0#]] 3])
	 (hash #1=[1 #1# [2 [[1 #1# [2 [#1#]] 3]]] 3])))

(assert (not (equal?
	      (hash #0=[1 #0# [2 [#0#]] 3])
	      (hash #1=[6 #1# [2 [[1 #1# [2 [#1#]] 3]]] 3]))))

(assert (equal?
	 (hash [1 [2 [[1 1 [2 [1]] 3]]] 3])
	 (hash [1 [2 [[1 1 [2 [1]] 3]]] 3])))

(assert (not (equal?
	      (hash [6 1 [2 [[3 1 [2 [1]] 3]]] 3])
	      (hash [6 1 [2 [[1 1 [2 [1]] 3]]] 3]))))

(princ "all tests pass\n")
#t
