; -*- scheme -*-
; femtoLisp standard library
; by Jeff Bezanson (C) 2009
; Distributed under the BSD License

(if (not (bound? 'eq))
    (begin
      (set-constant! 'eq       eq?)
      (set-constant! 'equal    equal?)))

; convert a sequence of body statements to a single expression.
; this allows define, defun, defmacro, let, etc. to contain multiple
; body expressions as in Common Lisp.
(set! f-body (lambda (e)
               (cond ((atom? e)       #f)
                     ((eq (cdr e) ()) (car e))
                     (#t              (cons 'begin e)))))

(set-syntax! 'define-macro
             (lambda (form . body)
               (list 'set-syntax! (list 'quote (car form))
                     (list 'lambda (cdr form) (f-body body)))))

(define-macro (define form . body)
  (if (symbol? form)
      (list 'set! form (car body))
      (list 'set! (car form) (list 'lambda (cdr form) (f-body body)))))

(define *output-stream* *stdout*)
(define *input-stream*  *stdin*)
(define (print . args)
  (apply io.print (cons *output-stream* args)))
(define (princ . args)
  (apply io.princ (cons *output-stream* args)))

(define (set s v) (eval (list 'set! s (list 'quote v))))

(define (map f lst)
  (if (atom? lst) lst
      (cons (f (car lst)) (map f (cdr lst)))))

(define-macro (label name fn)
  (list (list 'lambda (list name) (list 'set! name fn)) #f))

(define-macro (let binds . body)
  ((lambda (lname)
     (begin
       (if (symbol? binds)
	   (begin (set! lname binds)
		  (set! binds (car body))
		  (set! body (cdr body))))
       ((lambda (thelambda theargs)
	  (cons (if lname
		    (list 'label lname thelambda)
		    thelambda)
		theargs))
	(list 'lambda
	      (map (lambda (c) (if (pair? c) (car c) c)) binds)
	      (f-body body))
	(map (lambda (c) (if (pair? c) (cadr c) #f)) binds))))
   #f))

(define (nconc . lsts)
  (cond ((null? lsts) ())
        ((null? (cdr lsts)) (car lsts))
        ((null? (car lsts)) (apply nconc (cdr lsts)))
        (#t (prog1 (car lsts)
		   (set-cdr! (last (car lsts))
			     (apply nconc (cdr lsts)))))))

(define (append . lsts)
  (cond ((null? lsts) ())
        ((null? (cdr lsts)) (car lsts))
        (#t ((label append2 (lambda (l d)
			      (if (null? l) d
				  (cons (car l)
					(append2 (cdr l) d)))))
	     (car lsts) (apply append (cdr lsts))))))

(define (member item lst)
  (cond ((atom? lst) #f)
        ((equal?     (car lst) item) lst)
        (#t          (member item (cdr lst)))))
(define (memq item lst)
  (cond ((atom? lst) #f)
        ((eq?        (car lst) item) lst)
        (#t          (memq item (cdr lst)))))
(define (memv item lst)
  (cond ((atom? lst) #f)
        ((eqv?       (car lst) item) lst)
        (#t          (memv item (cdr lst)))))

(define (assoc item lst)
  (cond ((atom? lst) #f)
	((equal?     (caar lst) item) (car lst))
	(#t          (assoc item (cdr lst)))))
(define (assv item lst)
  (cond ((atom? lst) #f)
	((eqv?       (caar lst) item) (car lst))
	(#t          (assv item (cdr lst)))))

(define (macrocall? e) (and (symbol? (car e))
			    (symbol-syntax (car e))))

(define (function? x)
  (or (builtin? x)
      (and (pair? x) (eq (car x) 'lambda))))
(define procedure? function?)

(define (macroexpand-1 e)
  (if (atom? e) e
      (let ((f (macrocall? e)))
	(if f (apply f (cdr e))
	    e))))

; convert to proper list, i.e. remove "dots", and append
(define (append.2 l tail)
  (cond ((null? l)  tail)
        ((atom? l)  (cons l tail))
        (#t         (cons (car l) (append.2 (cdr l) tail)))))

(define (cadr x) (car (cdr x)))

;(set! *special-forms* '(quote cond if and or while lambda trycatch
;                        set! begin))

(define (macroexpand e)
  ((label mexpand
          (lambda (e env f)
            (begin
              (while (and (pair? e)
                          (not (member (car e) env))
                          (set! f (macrocall? e)))
                (set! e (apply f (cdr e))))
              (cond ((and (pair? e)
                          (not (eq (car e) 'quote)))
                     (let ((newenv
                            (if (and (eq (car e) 'lambda)
                                     (pair? (cdr e)))
                                (append.2 (cadr e) env)
                              env)))
                       (map (lambda (x) (mexpand x newenv ())) e)))
                    ;((and (symbol? e) (constant? e)) (eval e))
                    ;((and (symbol? e)
                    ;      (not (member e *special-forms*))
                    ;      (not (member e env))) (cons '%top e))
                    (#t e)))))
   e () ()))

(define (delete-duplicates lst)
  (if (atom? lst)
      lst
      (let ((elt  (car lst))
	    (tail (cdr lst)))
	(if (member elt tail)
	    (delete-duplicates tail)
	    (cons elt
		  (delete-duplicates tail))))))

(define (get-defined-vars- expr)
  (cond ((atom? expr) ())
	((and (eq? (car expr) 'define)
	      (pair? (cdr expr)))
	 (or (and (symbol? (cadr expr))
		  (list (cadr expr)))
	     (and (pair? (cadr expr))
		  (symbol? (caadr expr))
		  (list (caadr expr)))
	     ()))
	((eq? (car expr) 'begin)
	 (apply append (map get-defined-vars- (cdr expr))))
	(else ())))
(define (get-defined-vars expr)
  (delete-duplicates (get-defined-vars- expr)))

; redefine f-body to support internal defines
(define f-body- f-body)
(define (f-body e)
  ((lambda (B)
     ((lambda (V)
	(if (null? V)
	    B
	    (cons (list 'lambda V B) (map (lambda (x) #f) V))))
      (get-defined-vars B)))
   (f-body- e)))

(define-macro (body . forms) (f-body forms))

(define (expand x) (macroexpand x))

(define =   eqv?)
(define (/= a b) (not (eqv? a b)))
(define (>  a b) (< b a))
(define (<= a b) (not (< b a)))
(define (>= a b) (not (< a b)))
(define (negative? x) (< x 0))
(define (zero? x)     (= x 0))
(define (positive? x) (> x 0))
(define (even? x) (= (logand x 1) 0))
(define (odd? x) (not (even? x)))
(define (1+ n) (+ n 1))
(define (1- n) (- n 1))
(define (mod x y) (- x (* (/ x y) y)))
(define remainder mod)
(define (abs x)   (if (< x 0) (- x) x))
(define (identity x) x)
(define (char? x) (eq? (typeof x) 'wchar))
(define K prog1)  ; K combinator ;)
(define begin0 prog1)

(define (caar x) (car (car x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))
(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))

(define (every pred lst)
  (or (atom? lst)
      (and (pred (car lst))
           (every pred (cdr lst)))))

(define (any pred lst)
  (and (pair? lst)
       (or (pred (car lst))
           (any pred (cdr lst)))))

(define (listp a) (or (null? a) (pair? a)))
(define (list? a) (or (null? a) (and (pair? a) (list? (cdr a)))))

(define (nthcdr lst n)
  (if (<= n 0) lst
      (nthcdr (cdr lst) (- n 1))))
(define list-tail nthcdr)

(define (list-ref lst n)
  (car (nthcdr lst n)))

; bounded length test
; use this instead of (= (length lst) n), since it avoids unnecessary
; work and always terminates.
(define (length= lst n)
  (cond ((< n 0)     #f)
	((= n 0)     (null? lst))
	((null? lst) (= n 0))
	(else        (length= (cdr lst) (- n 1)))))

(define (list* . l)
  (if (atom? (cdr l))
      (car l)
      (cons (car l) (apply list* (cdr l)))))

(define (nlist* . l)
  (if (atom? (cdr l))
      (car l)
      (set-cdr! l (apply nlist* (cdr l)))))

(define (lastcdr l)
  (if (atom? l) l
      (lastcdr (cdr l))))

(define (last l)
  (cond ((atom? l)        l)
        ((atom? (cdr l))  l)
        (#t               (last (cdr l)))))
(define last-pair last)

(define (map! f lst)
  (prog1 lst
	 (while (pair? lst)
		(set-car! lst (f (car lst)))
		(set! lst (cdr lst)))))

(define (mapcar f . lsts)
  ((label mapcar-
          (lambda (f lsts)
            (cond ((null? lsts) (f))
                  ((atom? (car lsts)) (car lsts))
                  (#t (cons (apply   f (map car lsts))
			    (mapcar- f (map cdr lsts)))))))
   f lsts))

(define (transpose M) (apply mapcar (cons list M)))

(define (filter pred lst) (filter- pred lst ()))
(define (filter- pred lst accum)
  (cond ((null? lst) accum)
        ((pred (car lst))
         (filter- pred (cdr lst) (cons (car lst) accum)))
        (#t
         (filter- pred (cdr lst) accum))))

(define (separate pred lst) (separate- pred lst () ()))
(define (separate- pred lst yes no)
  (cond ((null? lst) (cons yes no))
        ((pred (car lst))
         (separate- pred (cdr lst) (cons (car lst) yes) no))
        (#t
         (separate- pred (cdr lst) yes (cons (car lst) no)))))

(define (foldr f zero lst)
  (if (null? lst) zero
    (f (car lst) (foldr f zero (cdr lst)))))

(define (foldl f zero lst)
  (if (null? lst) zero
    (foldl f (f (car lst) zero) (cdr lst))))

(define (reverse lst) (foldl cons () lst))

(define (copy-list l)
  (if (atom? l) l
    (cons (car l)
          (copy-list (cdr l)))))
(define (copy-tree l)
  (if (atom? l) l
    (cons (copy-tree (car l))
          (copy-tree (cdr l)))))

(define (nreverse l)
  (let ((prev ()))
    (while (pair? l)
	   (set! l (prog1 (cdr l)
			  (set-cdr! l (prog1 prev
					     (set! prev l))))))
    prev))

(define-macro (let* binds . body)
  (cons (list 'lambda (map car binds)
              (f-body
	       (nconc (map (lambda (b) (cons 'set! b)) binds)
		      body)))
        (map (lambda (x) #f) binds)))
(set-syntax! 'letrec (symbol-syntax 'let*))

(define-macro (when   c . body) (list 'if c (f-body body) #f))
(define-macro (unless c . body) (list 'if c #f (f-body body)))

(define (revappend l1 l2) (nconc (reverse l1) l2))
(define (nreconc   l1 l2) (nconc (nreverse l1) l2))

(define (list->vector l) (apply vector l))
(define (vector->list v)
  (let ((n (length v))
        (l ()))
    (for 1 n
         (lambda (i)
           (set! l (cons (aref v (- n i)) l))))
    l))

(define (self-evaluating? x)
  (or (and (atom? x)
           (not (symbol? x)))
      (and (constant? x)
           (eq x (eval x)))))

; backquote
(define-macro (backquote x) (bq-process x))

(define (splice-form? x)
  (or (and (pair? x) (or (eq (car x) '*comma-at*)
                         (eq (car x) '*comma-dot*)))
      (eq x '*comma*)))

(define (bq-process x)
  (cond ((self-evaluating? x)
         (if (vector? x)
             (let ((body (bq-process (vector->list x))))
               (if (eq (car body) 'list)
                   (cons vector (cdr body))
                 (list apply vector body)))
           x))
        ((atom? x)                    (list 'quote x))
        ((eq (car x) 'backquote)      (bq-process (bq-process (cadr x))))
        ((eq (car x) '*comma*)        (cadr x))
        ((not (any splice-form? x))
         (let ((lc    (lastcdr x))
               (forms (map bq-bracket1 x)))
           (if (null? lc)
               (cons 'list forms)
             (nconc (cons 'nlist* forms) (list (bq-process lc))))))
        (#t (let ((p x) (q ()))
	      (while (and (pair? p)
			  (not (eq (car p) '*comma*)))
		     (set! q (cons (bq-bracket (car p)) q))
		     (set! p (cdr p)))
	      (let ((forms
		     (cond ((pair? p) (nreconc q (list (cadr p))))
			   ((null? p)  (nreverse q))
			   (#t        (nreconc q (list (bq-process p)))))))
		(if (null? (cdr forms))
		    (car forms)
		    (cons 'nconc forms)))))))

(define (bq-bracket x)
  (cond ((atom? x)                  (list list (bq-process x)))
        ((eq (car x) '*comma*)      (list list (cadr x)))
        ((eq (car x) '*comma-at*)   (list 'copy-list (cadr x)))
        ((eq (car x) '*comma-dot*)  (cadr x))
        (#t                         (list list (bq-process x)))))

; bracket without splicing
(define (bq-bracket1 x)
  (if (and (pair? x) (eq (car x) '*comma*))
      (cadr x)
      (bq-process x)))

(define (quote-value v)
  (if (self-evaluating? v)
      v
      (list 'quote v)))

(define-macro (case key . clauses)
  (define (vals->cond key v)
    (cond ((eq? v 'else)   'else)
	  ((null? v)       #f)
          ((atom? v)       `(eqv? ,key ,(quote-value v)))
	  ((null? (cdr v)) `(eqv? ,key ,(quote-value (car v))))
	  (else            `(memv ,key ',v))))
  (let ((g (gensym)))
    `(let ((,g ,key))
       (cond ,@(map (lambda (clause)
		      (cons (vals->cond g (car clause))
			    (cdr clause)))
		    clauses)))))

(define-macro (do vars test-spec . commands)
  (let ((loop (gensym))
	(test-expr (car test-spec))
	(vars  (map car  vars))
	(inits (map cadr vars))
	(steps (map (lambda (x)
		      (if (pair? (cddr x))
			  (caddr x)
			  (car x)))
		    vars)))
    `(letrec ((,loop (lambda ,vars
		       (if ,test-expr
			   (begin
			     ,@(cdr test-spec))
			   (begin
			     ,@commands
			     (,loop ,@steps))))))
       (,loop ,@inits))))

(define-macro (dotimes var . body)
  (let ((v (car var))
        (cnt (cadr var)))
    `(for 0 (- ,cnt 1)
          (lambda (,v) ,(f-body body)))))

(define (map-int f n)
  (if (<= n 0)
      ()
    (let ((first (cons (f 0) ()))
          (acc ()))
      (set! acc first)
      (for 1 (- n 1)
           (lambda (i)
             (begin (set-cdr! acc (cons (f i) ()))
                    (set! acc (cdr acc)))))
      first)))

(define (iota n) (map-int identity n))
(define ι iota)

(define (for-each f l)
  (if (pair? l)
      (begin (f (car l))
	     (for-each f (cdr l)))
      #t))

(define (error . args) (raise (cons 'error args)))

(define-macro (throw tag value) `(raise (list 'thrown-value ,tag ,value)))
(define-macro (catch tag expr)
  (let ((e (gensym)))
    `(trycatch ,expr
               (lambda (,e) (if (and (pair? ,e)
                                     (eq (car  ,e) 'thrown-value)
                                     (eq (cadr ,e) ,tag))
                                (caddr ,e)
				(raise ,e))))))

(define-macro (unwind-protect expr finally)
  (let ((e (gensym)))
    `(prog1 (trycatch ,expr
                      (lambda (,e) (begin ,finally (raise ,e))))
	    ,finally)))

(if (or (eq? *os-name* 'win32)
	(eq? *os-name* 'win64)
	(eq? *os-name* 'windows))
    (begin (define *directory-separator* "\\")
	   (define *linefeed* "\r\n"))
    (begin (define *directory-separator* "/")
	   (define *linefeed* "\n")))

(define-macro (assert expr) `(if ,expr #t (raise '(assert-failed ,expr))))

(define-macro (time expr)
  (let ((t0 (gensym)))
    `(let ((,t0 (time.now)))
       (prog1
	,expr
	(princ "Elapsed time: " (- (time.now) ,t0) " seconds\n")))))

(define (terpri) (princ *linefeed*))
(define (display x) (princ x) #t)
(define (println . args) (prog1 (apply print args) (terpri)))

(define (vu8 . elts) (apply array (cons 'uint8 elts)))

(define (vector.map f v)
  (let* ((n (length v))
         (nv (vector.alloc n)))
    (for 0 (- n 1)
         (lambda (i)
           (aset! nv i (f (aref v i)))))
    nv))

(define (table.pairs t)
  (table.foldl (lambda (k v z) (cons (cons k v) z))
               () t))
(define (table.keys t)
  (table.foldl (lambda (k v z) (cons k z))
               () t))
(define (table.values t)
  (table.foldl (lambda (k v z) (cons v z))
               () t))
(define (table.clone t)
  (let ((nt (table)))
    (table.foldl (lambda (k v z) (put! nt k v))
                 () t)
    nt))

(define (load filename)
  (let ((F (file filename :read)))
    (trycatch
     (let next (prev E v)
       (if (not (io.eof? F))
	   (next (read F)
                 prev
		 (eval (expand E)))
	   (begin (io.close F)
		  ; evaluate last form in almost-tail position
		  (eval (expand E)))))
     (lambda (e)
       (begin
	 (io.close F)
	 (raise `(load-error ,filename ,e)))))))

(define (string.tail s n)
  (string.sub s (string.inc s 0 n) (sizeof s)))

(define *banner* (string.tail "
;  _
; |_ _ _ |_ _ |  . _ _
; | (-||||_(_)|__|_)|_)
;-------------------|----------------------------------------------------------

" 1))

(define *whitespace*
  (string.encode #array(wchar 9 10 11 12 13 32 133 160 5760 6158 8192
			      8193 8194 8195 8196 8197 8198 8199 8200
			      8201 8202 8232 8233 8239 8287 12288)))

(define (string.trim s at-start at-end)
  (define (trim-start s chars i L)
    (if (and (< i L)
	     (string.find chars (string.char s i)))
	(trim-start s chars (string.inc s i) L)
	i))
  (define (trim-end s chars i)
    (if (and (> i 0)
	     (string.find chars (string.char s (string.dec s i))))
	(trim-end s chars (string.dec s i))
	i))
  (let ((L (length s)))
    (string.sub s
		(trim-start s at-start 0 L)
		(trim-end   s at-end   L))))

(define (string.map f s)
  (let ((b (buffer))
	(n (length s)))
    (let ((i 0))
      (while (< i n)
	     (begin (io.putc b (f (string.char s i)))
		    (set! i (string.inc s i)))))
    (io.tostring! b)))

(define (print-to-string v)
  (let ((b (buffer)))
    (io.print b v)
    (io.tostring! b)))

(define (io.readline s) (io.readuntil s #byte(0xA)))

(define (repl)
  (define (prompt)
    (princ "> ") (io.flush *output-stream*)
    (let ((v (trycatch (read)
		       (lambda (e) (begin (io.discardbuffer *input-stream*)
					  (raise e))))))
      (and (not (io.eof? *input-stream*))
	   (let ((V (eval v)))
	     (print V)
	     (set! that V)
	     #t))))
  (define (reploop)
    (when (trycatch (and (prompt) (terpri))
		    print-exception)
	  (begin (terpri)
		 (reploop))))
  (reploop)
  (terpri))

(define (print-exception e)
  (cond ((and (pair? e)
	      (eq? (car e) 'type-error)
	      (length= e 4))
	 (io.princ *stderr*
		   "type-error: " (cadr e) ": expected " (caddr e) ", got ")
	 (io.print *stderr* (cadddr e)))

	((and (pair? e)
	      (eq? (car e) 'unbound-error)
	      (pair? (cdr e)))
	 (io.princ *stderr*
		   "unbound-error: eval: variable " (cadr e)
		   " has no value"))

	((and (pair? e)
	      (eq? (car e) 'error))
	 (io.princ *stderr* "error: ")
	 (apply io.princ (cons *stderr* (cdr e))))

	((and (pair? e)
	      (eq? (car e) 'load-error))
	 (print-exception (caddr e))
	 (io.princ *stderr* "in file " (cadr e)))

	((and (list? e)
	      (length= e 2))
	 (io.princ *stderr* (car e) ": ")
	 (let ((msg (cadr e)))
	   ((if (or (string? msg) (symbol? msg))
		io.princ io.print)
	    *stderr* msg)))

	(else (io.princ *stderr* "*** Unhandled exception: ")
	      (io.print *stderr* e)))

  (io.princ *stderr* *linefeed*)
  #t)

(define (__script fname)
  (trycatch (load fname)
	    (lambda (e) (begin (print-exception e)
			       (exit 1)))))

(define (__start . argv)
  ; reload this file with our new definition of load
  (load (string *install-dir* *directory-separator* "system.lsp"))
  (if (pair? (cdr argv))
      (begin (set! *argv* (cdr argv))
	     (__script (cadr argv)))
      (begin (set! *argv* argv)
	     (princ *banner*)
	     (repl)))
  (exit 0))
