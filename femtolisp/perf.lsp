(load "test.lsp")

(princ "colorgraph: ")
(load "tcolor.lsp")

(princ "fib(34): ")
(assert (equal? (time (fib 34)) 5702887))
(princ "yfib(32): ")
(assert (equal? (time (yfib 32)) 2178309))

(princ "sort: ")
(set! r (map-int (lambda (x) (mod (+ (* x 9421) 12345) 1024)) 1000))
(time (simple-sort r))

(princ "expand: ")
(time (dotimes (n 5000) (expand '(dotimes (i 100) body1 body2))))

(define (my-append . lsts)
  (cond ((null? lsts) ())
        ((null? (cdr lsts)) (car lsts))
        (else (letrec ((append2 (lambda (l d)
				  (if (null? l) d
				      (cons (car l)
					    (append2 (cdr l) d))))))
		(append2 (car lsts) (apply my-append (cdr lsts)))))))

(princ "append: ")
(set! L (map-int (lambda (x) (map-int identity 20)) 20))
(time (dotimes (n 1000) (apply my-append L)))

(path.cwd "ast")
(princ "p-lambda: ")
(load "rpasses.lsp")
(define *input* (load "datetimeR.lsp"))
(time (set! *output* (compile-ish *input*)))
(assert (equal? *output* (load "rpasses-out.lsp")))
(path.cwd "..")
