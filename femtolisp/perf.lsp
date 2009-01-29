(load "test.lsp")

(princ "colorgraph: ")
(load "tcolor.lsp")

(princ "fib(34): ")
(assert (equal (time (fib 34)) 5702887))
(princ "yfib(32): ")
(assert (equal (time (yfib 32)) 2178309))

(princ "sort: ")
(set! r (map-int (lambda (x) (mod (+ (* x 9421) 12345) 1024)) 1000))
(time (sort r))

(princ "mexpand: ")
(time (dotimes (n 5000) (macroexpand '(dotimes (i 100) body1 body2))))

(princ "append: ")
(set! L (map-int (lambda (x) (map-int identity 20)) 20))
(time (dotimes (n 1000) (apply append L)))

(path.cwd "ast")
(princ "p-lambda: ")
(load "rpasses.lsp")
(define *input* (load "datetimeR.lsp"))
(time (set! *output* (compile-ish *input*)))
(assert (equal *output* (load "rpasses-out.lsp")))
(path.cwd "..")
