(define (bq-process2 x d)
  (define (splice-form? x)
    (or (and (pair? x) (or (eq? (car x) 'unquote-splicing)
			   (eq? (car x) 'unquote-nsplicing)
			   (and (eq? (car x) 'unquote)
				(length> x 2))))
	(eq? x 'unquote)))
  ;; bracket without splicing
  (define (bq-bracket1 x)
    (if (and (pair? x) (eq? (car x) 'unquote))
	(if (= d 0)
	    (cadr x)
	    (list cons ''unquote
		  (bq-process2 (cdr x) (- d 1))))
	(bq-process2 x d)))
  (define (bq-bracket x)
    (cond ((atom? x)  (list list (bq-process2 x d)))
	  ((eq? (car x) 'unquote)
	   (if (= d 0)
	       (cons list (cdr x))
	       (list list (list cons ''unquote
				(bq-process2 (cdr x) (- d 1))))))
	  ((eq? (car x) 'unquote-splicing)
	   (if (= d 0)
	       (list 'copy-list (cadr x))
	       (list list (list list ''unquote-splicing
				(bq-process2 (cadr x) (- d 1))))))
	  ((eq? (car x) 'unquote-nsplicing)
	   (if (= d 0)
	       (cadr x)
	       (list list (list list ''unquote-nsplicing
				(bq-process2 (cadr x) (- d 1))))))
	  (else  (list list (bq-process2 x d)))))
  (cond ((symbol? x)  (list 'quote x))
	((vector? x)
	 (let ((body (bq-process2 (vector->list x) d)))
	   (if (eq? (car body) list)
	       (cons vector (cdr body))
	       (list apply vector body))))
        ((atom? x)  x)
        ((eq? (car x) 'quasiquote)
	 (list list ''quasiquote (bq-process2 (cadr x) (+ d 1))))
        ((eq? (car x) 'unquote)
	 (if (and (= d 0) (length= x 2))
	     (cadr x)
	     (list cons ''unquote (bq-process2 (cdr x) (- d 1)))))
	((or (> d 0) (not (any splice-form? x)))
         (let ((lc    (lastcdr x))
               (forms (map bq-bracket1 x)))
           (if (null? lc)
               (cons list forms)
	       (if (null? (cdr forms))
		   (list cons (car forms) (bq-process2 lc d))
		   (nconc (cons list* forms) (list (bq-process2 lc d)))))))
	(else
	 (let loop ((p x) (q ()))
	   (cond ((null? p) ;; proper list
		  (cons 'nconc (reverse! q)))
		 ((pair? p)
		  (cond ((eq? (car p) 'unquote)
			 ;; (... . ,x)
			 (cons 'nconc
			       (nreconc q
					(if (= d 0)
					    (cdr p)
					    (list (list list ''unquote)
						  (bq-process2 (cdr p)
							       (- d 1)))))))
			(else
			 (loop (cdr p) (cons (bq-bracket (car p)) q)))))
		 (else
		  ;; (... . x)
		  (cons 'nconc (reverse! (cons (bq-process2 p d) q)))))))))

#|
tests

> ``(,a ,,a ,b ,@b ,,@b)
`(,a ,1 ,b ,@b (unquote 2 3))
> `(,a ,1 ,b ,@b (unquote 2 3))
(1 1 (2 3) 2 3 2 3)

(define a 1)

(bq-process2 '`(,a (unquote unquote a)) 0)

(define b '(unquote a))
(define unquote 88)
(bq-process2 '``(,a ,,,@b) 0)
; etc. => (1 88 1)

(define b '(a a))
(bq-process2 '``(,a ,,,@b) 0)
; etc. => (1 1 1)
|#

;; minimal version with no optimizations, vectors, or dotted lists
(define (bq-process0 x d)
  (define (bq-bracket x)
    (cond ((and (pair? x) (eq? (car x) 'unquote))
	   (if (= d 0)
	       (cons list (cdr x))
	       (list list (list cons ''unquote
				(bq-process0 (cdr x) (- d 1))))))
	  ((and (pair? x) (eq? (car x) 'unquote-splicing))
	   (if (= d 0)
	       (list 'copy-list (cadr x))
	       (list list (list list ''unquote-splicing
				(bq-process0 (cadr x) (- d 1))))))
	  (else  (list list (bq-process0 x d)))))
  (cond ((symbol? x)  (list 'quote x))
        ((atom? x)    x)
        ((eq? (car x) 'quasiquote)
	 (list list ''quasiquote (bq-process0 (cadr x) (+ d 1))))
        ((eq? (car x) 'unquote)
	 (if (and (= d 0) (length= x 2))
	     (cadr x)
	     (list cons ''unquote (bq-process0 (cdr x) (- d 1)))))
	(else
	 (cons 'nconc (map bq-bracket x)))))

#t
