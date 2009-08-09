(define ones (map (lambda (x) 1) (iota 1000000)))

(write (apply + ones))
(newline)

(define (big n)
  (if (<= n 0)
      0
      `(+ 1 1 1 1 1 1 1 1 1 1 ,(big (- n 1)))))

(define nst (big 100000))

(write (eval nst))
(newline)

(define longg (cons '+ ones))
(write (eval longg))
(newline)

(define (f x)
  (begin (write x)
	 (newline)
	 (f (+ x 1))
	 0))
