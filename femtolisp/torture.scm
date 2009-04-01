(define (big n)
  (if (<= n 0)
      0
      `(+ 1 1 1 1 1 1 1 1 1 1 ,(big (- n 1)))))

(define nst `(display ,(big 100000)))

(display (eval nst))
(newline)

(define (f x)
  (begin (display x)
	 (newline)
	 (f (+ x 1))
	 0))

(define longg (cons '+ (map (lambda (x) 1) (iota 1000000))))
(display (eval longg))
(newline)
