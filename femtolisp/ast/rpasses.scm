(include "iscutil.scm")
(include "match.scm")
(include "asttools.scm")
;(load "plambda-js.scm")
;(load "plambda-chez.scm")

;(pretty-print *input*)

#|
Overall phases:
I.   s-expr output
II.  tree normalization
       1. control construct normalization, flattening. various restructuring.
       2. transformations that might add variables
       3. local variable detection
III. var/func attribute analysis
IV.  argument normalization
V.   type inference
       1. split each function into generic/non-generic versions. the generic
          one resolves generic funcs to calls to a lookup routine that tries
          to find stuff like `diag<-.darray`. the other one assumes everything
          is handled by a builtin R function with a known t-function
       2. inference
VI.  code generation

Useful R lowering passes:

- control construct normalization
  . convert while/repeat/various for forms/break/next to while/break
  . convert switch to nested if

- local variable detection
  . classify vars as (1) definitely local, (2) possibly-local, (3) free
  . collect all local or possibly-local vars and wrap the body with
    (let ((g0 (upvalue 'var1))
          (g1 (upvalue 'var2)))
      <body>)

    where (upvalue x) is either (get-global x) or (captured-var n i)
    for definitely-local, start as null instead of upvalue

    then we have to rename var1 to g0 everywhere inside that.
    for the vast majority of functions that don't attempt to modify parent-scope
    locals, pure-functional closure conversion would work.

    utility for this: fold-along-cfg
  . after this the tree is ready for typical lexical scope analysis

 (- closure conversion/deBruijn indices)

- argument normalization for call to known function
  . convert lambda arglist to plain list of symbols
  . move default initializers into body as `(when (eq? ,argname 'missing) ,assign)
  . at call site sort args to correct positions, add explicit missing
  . if call target unknown insert call to match.args or whatever

- r-block, ||, && flattening

- fancy assignment transformation:
  f(v) <- rhs,  (<- (r-call f v) rhs)
  performs:
  (begin (<- v (r-call f<- v rhs))
         rhs)

- (<- a b) becomes (ref= a (lazy-copy b))
  arguments to functions are wrapped in lazy-copy at the call site, so we can
  omit the copy (1) for functions marked as pass-by-ref, (2) where user indicated
  pass-by-ref, (3) for arguments which are strictly-allocating expressions,
  (4) for user functions proven to be ref-safe and thus marked as case (1)

Useful analyses:

- prove function strictness!!
  . strict functions need to open with (if (promise? arg) (force arg) arg) for each
    arg, in case they are called indirectly.
- prove global variables constant (esp. function names)
  . prove builtins redefined/constant
- need dictionary of builtin properties (pure/strict/t-functions/etc.)
- useful but very general types:
  single: has length 1 and no attrs (implies simple)
  simple: has default class attributes
  array: has dim attribute only
  distributed: starp array
  numeric
|#


(define missing-arg-tag '*r-missing*)

; tree inspection utils

(define (assigned-var e)
  (and (pair? e)
       (or (eq? (car e) '<-) (eq? (car e) 'ref=))
       (symbol? (cadr e))
       (cadr e)))

(define (func-argnames f)
  (let ((argl (cadr f)))
    (if (eq? argl '*r-null*) ()
	(map cadr argl))))

; transformations

(define (dollarsign-transform e)
  (pattern-expand
   (pattern-lambda ($ lhs name)
		   (let* ((g (if (not (pair? lhs)) lhs (gensym)))
			  (n (if (symbol? name)
				 (symbol->string name)
				 name))
			  (expr `(r-call
				  r-aref ,g (index-in-strlist ,n (r-call attr ,g "names")))))
		     (if (not (pair? lhs))
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
		   (let ((g  (if (pair? rhs) (gensym) rhs))
			 (op (car __)))
		     `(r-block ,@(if (pair? rhs) `((ref= ,g ,rhs)) ())
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
       (filter (lambda (arg) (not (eq? (caddr arg) missing-arg-tag))) arglist)))

; convert r function expressions to lambda
(define (normalize-r-functions e)
  (maptree-post (lambda (n)
		  (if (and (pair? n) (eq? (car n) 'function))
		      `(lambda ,(func-argnames n)
			 (r-block ,@(gen-default-inits (cadr n))
				  ,@(if (and (pair? (caddr n))
					     (eq? (car (caddr n)) 'r-block))
					(cdr (caddr n))
					(list (caddr n)))))
		      n))
		e))

(define (find-assigned-vars n)
  (let ((vars ()))
    (maptree-pre (lambda (s)
		   (if (not (pair? s)) s
		       (cond ((eq? (car s) 'lambda) #f)
			     ((eq? (car s) '<-)
			      (set! vars (list-adjoin (cadr s) vars))
			      (cddr s))
			     (else s))))
		 n)
    vars))

; introduce let based on assignment statements
(define (letbind-locals e)
    (maptree-post (lambda (n)
		    (if (and (pair? n) (eq? (car n) 'lambda))
			(let ((vars (find-assigned-vars (cddr n))))
			  `(lambda ,(cadr n) (let ,(map list
							vars
							(map (lambda (x) '()) vars))
					       ,@(cddr n))))
			n))
		  e))

(define (compile-ish e)
  (letbind-locals
   (normalize-r-functions
    (fancy-assignment-transform
     (dollarsign-transform
      (flatten-all-op && (flatten-all-op || e)))))))

;(trace map)
;(pretty-print (compile-ish *input*))
;(print
; (time-call (lambda () (compile-ish *input*)) 1)
;)
(define (main)
  (begin
    (define *input* (read))
    (define t0 ((java.util.Date:new):getTime))
    (compile-ish *input*)
    (define t1 ((java.util.Date:new):getTime))
    (display "milliseconds: ")
    (display (- t1 t0))
    (newline)))

(main)
