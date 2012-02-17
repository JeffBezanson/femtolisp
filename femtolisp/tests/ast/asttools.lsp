; -*- scheme -*-
; utilities for AST processing

(define (symconcat s1 s2)
  (symbol (string s1 s2)))

(define (list-adjoin item lst)
  (if (member item lst)
      lst
    (cons item lst)))

(define (index-of item lst start)
  (cond ((null? lst) #f)
	((eq item (car lst)) start)
	(#t (index-of item (cdr lst) (+ start 1)))))

(define (each f l)
  (if (null? l) l
    (begin (f (car l))
           (each f (cdr l)))))

(define (maptree-pre f tr)
  (let ((new-t (f tr)))
    (if (pair? new-t)
        (map (lambda (e) (maptree-pre f e)) new-t)
      new-t)))

(define (maptree-post f tr)
  (if (not (pair? tr))
      (f tr)
    (let ((new-t (map (lambda (e) (maptree-post f e)) tr)))
      (f new-t))))

(define (foldtree-pre f t zero)
  (if (not (pair? t))
      (f t zero)
      (foldl t (lambda (e state) (foldtree-pre f e state)) (f t zero))))

(define (foldtree-post f t zero)
  (if (not (pair? t))
      (f t zero)
      (f t (foldl t (lambda (e state) (foldtree-post f e state)) zero))))

; general tree transformer
; folds in preorder (foldtree-pre), maps in postorder (maptree-post)
; therefore state changes occur immediately, just by looking at the current node,
; while transformation follows evaluation order. this seems to be the most natural
; approach.
; (mapper tree state) - should return transformed tree given current state
; (folder tree state) - should return new state
(define (map&fold t zero mapper folder)
  (let ((head (and (pair? t) (car t))))
    (cond ((eq? head 'quote)
	   t)
	  ((or (eq? head 'the) (eq? head 'meta))
	   (list head
		 (cadr t)
		 (map&fold (caddr t) zero mapper folder)))
	  (else
	   (let ((new-s (folder t zero)))
	     (mapper
	      (if (pair? t)
		  ; head symbol is a tag; never transform it
		  (cons (car t)
			(map (lambda (e) (map&fold e new-s mapper folder))
			     (cdr t)))
		  t)
	      new-s))))))

; convert to proper list, i.e. remove "dots", and append
(define (append.2 l tail)
  (cond ((null? l)  tail)
        ((atom? l)  (cons l tail))
        (#t         (cons (car l) (append.2 (cdr l) tail)))))

; transform code by calling (f expr env) on each subexpr, where
; env is a list of lexical variables in effect at that point.
(define (lexical-walk f t)
  (map&fold t () f
	    (lambda (tree state)
	      (if (and (eq? (car t) 'lambda)
		       (pair? (cdr t)))
		  (append.2 (cadr t) state)
		  state))))

; collapse forms like (&& (&& (&& (&& a b) c) d) e) to (&& a b c d e)
(define (flatten-left-op op e)
  (maptree-post (lambda (node)
                  (if (and (pair? node)
                           (eq (car node) op)
                           (pair? (cdr node))
                           (pair? (cadr node))
                           (eq (caadr node) op))
                      (cons op
                            (append (cdadr node) (cddr node)))
                    node))
                e))

; convert all local variable references to (lexref rib slot name)
; where rib is the nesting level and slot is the stack slot#
; name is just there for reference
; this assumes lambda is the only remaining naming form
(define (lookup-var v env lev)
  (if (null? env) v
    (let ((i (index-of v (car env) 0)))
      (if i (list 'lexref lev i v)
        (lookup-var v (cdr env) (+ lev 1))))))
(define (lvc- e env)
  (cond ((symbol? e) (lookup-var e env 0))
        ((pair? e)
         (if (eq (car e) 'quote)
             e
	     (let* ((newvs (and (eq (car e) 'lambda) (cadr e)))
		    (newenv (if newvs (cons newvs env) env)))
	       (if newvs
		   (cons 'lambda
			 (cons (cadr e)
			       (map (lambda (se) (lvc- se newenv))
				    (cddr e))))
		   (map (lambda (se) (lvc- se env)) e)))))
        (#t e)))
(define (lexical-var-conversion e)
  (lvc- e ()))

; convert let to lambda
(define (let-expand e)
  (maptree-post (lambda (n)
		  (if (and (pair? n) (eq (car n) 'let))
		      `((lambda ,(map car (cadr n)) ,@(cddr n))
			,@(map cadr (cadr n)))
                    n))
		e))

; alpha renaming
; transl is an assoc list ((old-sym-name . new-sym-name) ...)
(define (alpha-rename e transl)
  (map&fold e
	    ()
	    ; mapper: replace symbol if unbound
	    (lambda (t env)
	      (if (symbol? t)
		  (let ((found (assq t transl)))
		    (if (and found
			     (not (memq t env)))
			(cdr found)
			t))
		  t))
	    ; folder: add locals to environment if entering a new scope
	    (lambda (t env)
	      (if (and (pair? t) (or (eq? (car t) 'let)
				     (eq? (car t) 'lambda)))
		  (append (cadr t) env)
		  env))))

; flatten op with any associativity
(define-macro (flatten-all-op op e)
  `(pattern-expand
    (pattern-lambda (,op (-- l ...) (-- inner (,op ...)) (-- r ...))
                    (cons ',op (append l (cdr inner) r)))
    ,e))

(define-macro (pattern-lambda pat body)
  (let* ((args (patargs pat))
         (expander `(lambda ,args ,body)))
    `(lambda (expr)
       (let ((m (match ',pat expr)))
         (if m
             ; matches; perform expansion
             (apply ,expander (map (lambda (var) (cdr (or (assq var m) '(0 . #f))))
                                   ',args))
           #f)))))
