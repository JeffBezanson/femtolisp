(load "test.lsp")

(princ "colorgraph: ")
(load "tcolor.lsp")

(princ "fib(34): ")
(assert (equal (time (fib 34)) 5702887))
(princ "yfib(32): ")
(assert (equal (time (yfib 32)) 2178309))

(princ "sort: ")
(setq r (map-int (lambda (x) (mod (+ (* x 9421) 12345) 1024)) 1000))
(time (sort r))

(princ "mexpand: ")
(time (dotimes (n 5000) (macroexpand '(dotimes (i 100) body1 body2))))

(path.cwd "ast")
(princ "p-lambda: ")
(load "rpasses.lsp")
(path.cwd "..")
