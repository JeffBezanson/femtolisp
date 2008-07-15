(load "match.lsp")
(load "asttools.lsp")

(define missing-arg-tag '*r-missing*)

; tree inspection utils

(define (assigned-var e)
  (and (consp e)
       (or (eq (car e) '<-) (eq (car e) 'ref=))
       (symbolp (cadr e))
       (cadr e)))

(define (func-argnames f)
  (let ((argl (cadr f)))
    (if (eq argl '*r-null*) ()
        (map cadr argl))))

; transformations

(define (dollarsign-transform e)
  (pattern-expand
   (pattern-lambda ($ lhs name)
		   (let* ((g (if (not (consp lhs)) lhs (gensym)))
			  (n (if (symbolp name)
				 name ;(symbol->string name)
                               name))
			  (expr `(r-call
				  r-aref ,g (index-in-strlist ,n (r-call attr ,g "names")))))
		     (if (not (consp lhs))
			 expr
                       `(r-block (ref= ,g ,lhs) ,expr))))
   e))

; lower r expressions of the form  f(lhs,...) <- rhs
; TODO: if there are any special forms that can be f in this expression,
; they need to be handled separately. For example a$b can be lowered
; to an index assignment (by dollarsign-transform), after which
; this transform applies. I don't think there are any others though.
(define (fancy-assignment-transform e)
  (pattern-expand
   (pattern-lambda (-$ (<-  (r-call f lhs ...) rhs)
                       (<<- (r-call f lhs ...) rhs))
		   (let ((g  (if (consp rhs) (gensym) rhs))
                         (op (car __)))
		     `(r-block ,@(if (consp rhs) `((ref= ,g ,rhs)) ())
                               (,op ,lhs (r-call ,(symconcat f '<-) ,@(cddr (cadr __)) ,g))
                               ,g)))
   e))

; map an arglist with default values to appropriate init code
; function(x=blah) { ... } gets
;   if (missing(x)) x = blah
; added to its body
(define (gen-default-inits arglist)
  (map (lambda (arg)
	 (let ((name    (cadr arg))
	       (default (caddr arg)))
	   `(when (missing ,name)
              (<- ,name ,default))))
       (filter (lambda (arg) (not (eq (caddr arg) missing-arg-tag))) arglist)))

; convert r function expressions to lambda
(define (normalize-r-functions e)
  (maptree-post (lambda (n)
		  (if (and (consp n) (eq (car n) 'function))
		      `(lambda ,(func-argnames n)
			 (r-block ,@(gen-default-inits (cadr n))
				  ,@(if (and (consp (caddr n))
					     (eq (car (caddr n)) 'r-block))
					(cdr (caddr n))
                                      (list (caddr n)))))
                    n))
		e))

(define (find-assigned-vars n)
  (let ((vars ()))
    (maptree-pre (lambda (s)
		   (if (not (consp s)) s
                     (cond ((eq (car s) 'lambda) nil)
                           ((eq (car s) '<-)
                            (setq vars (list-adjoin (cadr s) vars))
                            (cddr s))
                           (T s))))
		 n)
    vars))

; introduce let based on assignment statements
(define (letbind-locals e)
  (maptree-post (lambda (n)
                  (if (and (consp n) (eq (car n) 'lambda))
                      (let ((vars (find-assigned-vars (cddr n))))
                        `(lambda ,(cadr n) (let ,(map (lambda (v) (list v ()))
                                                      vars)
                                             ,@(cddr n))))
                    n))
                e))

(define (compile-ish e)
  (letbind-locals
   (normalize-r-functions
    (fancy-assignment-transform
     (dollarsign-transform
      (flatten-all-op && (flatten-all-op \|\| e)))))))

;(trace map)
;(pretty-print (compile-ish *input*))
;(print
; (time-call (lambda () (compile-ish *input*)) 1)
;)
(define (main)
  (progn
    (define *input* (load "starpR.lsp"))
    ;(define t0 ((java.util.Date:new):getTime))
    (time (compile-ish *input*))
    ;(define t1 ((java.util.Date:new):getTime))
))

(main)
