; utilities for AST processing

(define (symconcat s1 s2)
  (string->symbol (string-append (symbol->string s1)
				 (symbol->string s2))))

(define (list-adjoin item lst)
  (if (memq item lst)
      lst
      (cons item lst)))

(define (index-of item lst start)
  (cond ((null? lst) #f)
	((eq? item (car lst)) start)
	(else (index-of item (cdr lst) (+ start 1)))))

(define (map! f l)
  (define (map!- f l start)
    (if (pair? l)
	(begin (set-car! l (f (car l)))
	       (map!- f (cdr l) start))
	start))
  (map!- f l l))

(define (each f l)
  (if (null? l) l
      (begin (f (car l))
	     (each f (cdr l)))))

(define (maptree-pre f t)
  (let ((new-t (f t)))
    (if (pair? new-t)
	(map (lambda (e) (maptree-pre f e)) new-t)
	new-t)))

(define (maptree-post f t)
  (if (not (pair? t))
      (f t)
      (let ((new-t (map (lambda (e) (maptree-post f e)) t)))
	(f new-t))))

; collapse forms like (&& (&& (&& (&& a b) c) d) e) to (&& a b c d e)
(define (flatten-left-op op e)
  (maptree-post (lambda (node)
		  (if (and (pair? node)
		           (eq? (car node) op)
			   (pair? (cdr node))
			   (pair? (cadr node))
			   (eq? (caadr node) op))
		      (cons op
			    (append (cdadr node) (cddr node)))
		      node))
		e))

; convert all local variable references to (lexref rib slot name)
; where rib is the nesting level and slot is the stack slot#
; name is just there for reference
; this assumes lambda is the only remaining naming form
(define (lexical-var-conversion e)
  (define (lookup-var v env lev)
    (if (null? env) v
	(let ((i (index-of v (car env) 0)))
	  (if i (list 'lexref lev i v)
	      (lookup-var v (cdr env) (+ lev 1))))))
  (define (lvc- e env)
    (cond ((symbol? e) (lookup-var e env 0))
	  ((pair? e)
	   (if (eq? (car e) 'quote)
	       e
	       (let* ((newvs (and (eq? (car e) 'lambda) (cadr e)))
		      (newenv (if newvs (cons newvs env) env)))
		 (if newvs
		     (cons 'lambda
			   (cons (cadr e)
				 (map (lambda (se) (lvc- se newenv))
				      (cddr e))))
		     (map (lambda (se) (lvc- se env)) e)))))
	  (else e)))
  (lvc- e ()))

; convert let to lambda
(define (let-expand e)
  (maptree-post (lambda (n)
		  (if (and (pair? n) (eq? (car n) 'let))
		      `((lambda ,(map car (cadr n)) ,@(cddr n))
			,@(map cadr (cadr n)))
		      n))
		e))
