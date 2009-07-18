; -*- scheme -*-
; femtoLisp standard library
; by Jeff Bezanson (C) 2009
; Distributed under the BSD License

(if (not (bound? '*syntax-environment*))
    (define *syntax-environment* (table)))

(define (set-syntax! s v) (put! *syntax-environment* s v))
(define (symbol-syntax s) (get *syntax-environment* s #f))

(define-macro (define-macro form . body)
  `(set-syntax! ',(car form)
		(lambda ,(cdr form) ,@body)))

(define-macro (label name fn)
  `((lambda (,name) (set! ,name ,fn)) #f))

(define (map f lst . lsts)
  (define (map1 f lst acc)
    (cdr
     (prog1 acc
      (while (pair? lst)
	     (begin (set! acc
			  (cdr (set-cdr! acc (cons (f (car lst)) ()))))
		    (set! lst (cdr lst)))))))
  (define (mapn f lsts)
    (if (null? (car lsts))
	()
	(cons (apply f (map1 car lsts (list ())))
	      (mapn  f (map1 cdr lsts (list ()))))))
  (if (null? lsts)
      (map1 f lst (list ()))
      (mapn f (cons lst lsts))))

(define-macro (let binds . body)
  (let (lname)
    (if (symbol? binds)
	(begin (set! lname binds)
	       (set! binds (car body))
	       (set! body (cdr body))))
    (let ((thelambda
	   `(lambda ,(map (lambda (c) (if (pair? c) (car c) c))
			  binds)
	      ,@body))
	  (theargs
	   (map (lambda (c) (if (pair? c) (cadr c) #f)) binds)))
      (cons (if lname
		`(label ,lname ,thelambda)
		thelambda)
	    theargs))))

(define-macro (letrec binds . body)
  `((lambda ,(map car binds)
      ,.(map (lambda (b) `(set! ,@b)) binds)
      ,@body)
    ,.(map (lambda (x) #f) binds)))

(define-macro (cond . clauses)
  (define (cond-clauses->if lst)
    (if (atom? lst)
	#f
	(let ((clause (car lst)))
	  (if (or (eq? (car clause) 'else)
		  (eq? (car clause) #t))
	      (if (null? (cdr clause))
		  (car clause)
		  (cons 'begin (cdr clause)))
	      (if (null? (cdr clause))
		  ; test by itself
		  (list 'or
			(car clause)
			(cond-clauses->if (cdr lst)))
		  (list 'if
			(car clause)
			(cons 'begin (cdr clause))
			(cond-clauses->if (cdr lst))))))))
  (cond-clauses->if clauses))

; standard procedures ---------------------------------------------------------

(define (member item lst)
  (cond ((atom? lst) #f)
        ((equal?     (car lst) item) lst)
        (#t          (member item (cdr lst)))))
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

(define (/= a b) (not (= a b)))
(define (>  a b) (< b a))
(define (<= a b) (or (< a b) (= a b)))
(define (>= a b) (or (< b a) (= a b)))
(define (negative? x) (< x 0))
(define (zero? x)     (= x 0))
(define (positive? x) (> x 0))
(define (even? x) (= (logand x 1) 0))
(define (odd? x) (not (even? x)))
(define (identity x) x)
(define (1+ n) (+ n 1))
(define (1- n) (- n 1))
(define (mod0 x y) (- x (* (div0 x y) y)))
(define (div x y) (+ (div0 x y)
		     (or (and (< x 0)
			      (or (and (< y 0) 1)
				  -1))
			 0)))
(define (mod x y) (- x (* (div x y) y)))
(define quotient div0)
(define remainder mod0)
(define (random n)
  (if (integer? n)
      (mod (rand) n)
      (* (rand.double) n)))
(define (abs x)   (if (< x 0) (- x) x))
(define (max x0 . xs)
  (if (null? xs) x0
      (foldl (lambda (a b) (if (< a b) b a)) x0 xs)))
(define (min x0 . xs)
  (if (null? xs) x0
      (foldl (lambda (a b) (if (< a b) a b)) x0 xs)))
(define (char? x) (eq? (typeof x) 'wchar))
(define (array? x) (or (vector? x)
		       (let ((t (typeof x)))
			 (and (pair? t) (eq? (car t) 'array)))))
(define (closure? x) (and (function? x) (not (builtin? x))))

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))
(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))

(let ((*values* (list '*values*)))
  (set! values
	(lambda vs
	  (if (and (pair? vs) (null? (cdr vs)))
	      (car vs)
	      (cons *values* vs))))
  (set! call-with-values
	(lambda (producer consumer)
	  (let ((res (producer)))
	    (if (and (pair? res) (eq? *values* (car res)))
		(apply consumer (cdr res))
		(consumer res))))))

; list utilities --------------------------------------------------------------

(define (every pred lst)
  (or (atom? lst)
      (and (pred (car lst))
           (every pred (cdr lst)))))

(define (any pred lst)
  (and (pair? lst)
       (or (pred (car lst))
           (any pred (cdr lst)))))

(define (list? a) (or (null? a) (and (pair? a) (list? (cdr a)))))

(define (list-tail lst n)
  (if (<= n 0) lst
      (list-tail (cdr lst) (- n 1))))

(define (list-head lst n)
  (if (<= n 0) ()
      (cons (car lst)
	    (list-head (cdr lst) (- n 1)))))

(define (list-ref lst n)
  (car (list-tail lst n)))

; bounded length test
; use this instead of (= (length lst) n), since it avoids unnecessary
; work and always terminates.
(define (length= lst n)
  (cond ((< n 0)     #f)
	((= n 0)     (atom? lst))
	((atom? lst) (= n 0))
	(else        (length= (cdr lst) (- n 1)))))

(define (length> lst n)
  (cond ((< n 0)     lst)
	((= n 0)     (and (pair? lst) lst))
	((atom? lst) (< n 0))
	(else        (length> (cdr lst) (- n 1)))))

(define (last-pair l)
  (if (atom? (cdr l))
      l
      (last-pair (cdr l))))

(define (lastcdr l)
  (if (atom? l)
      l
      (cdr (last-pair l))))

(define (to-proper l)
  (cond ((null? l) l)
	((atom? l) (list l))
	(else (cons (car l) (to-proper (cdr l))))))

(define (map! f lst)
  (prog1 lst
	 (while (pair? lst)
		(set-car! lst (f (car lst)))
		(set! lst (cdr lst)))))

(define filter
  (letrec ((filter-
	    (lambda (pred lst accum)
	      (cond ((null? lst) accum)
		    ((pred (car lst))
		     (filter- pred (cdr lst) (cons (car lst) accum)))
		    (#t
		     (filter- pred (cdr lst) accum))))))
    (lambda (pred lst) (filter- pred lst ()))))

(define separate
  (letrec ((separate-
	    (lambda (pred lst yes no)
	      (cond ((null? lst) (cons yes no))
		    ((pred (car lst))
		     (separate- pred (cdr lst) (cons (car lst) yes) no))
		    (#t
		     (separate- pred (cdr lst) yes (cons (car lst) no)))))))
    (lambda (pred lst) (separate- pred lst () ()))))

(define (count f l)
  (define (count- f l n)
    (if (null? l)
	n
	(count- f (cdr l) (if (f (car l))
			      (+ n 1)
			      n))))
  (count- f l 0))

(define (nestlist f zero n)
  (if (<= n 0) ()
      (cons zero (nestlist f (f zero) (- n 1)))))

(define (foldr f zero lst)
  (if (null? lst) zero
    (f (car lst) (foldr f zero (cdr lst)))))

(define (foldl f zero lst)
  (if (null? lst) zero
    (foldl f (f (car lst) zero) (cdr lst))))

(define (reverse lst) (foldl cons () lst))

(define (reverse! l)
  (let ((prev ()))
    (while (pair? l)
	   (set! l (prog1 (cdr l)
			  (set-cdr! l (prog1 prev
					     (set! prev l))))))
    prev))

(define (copy-tree l)
  (if (atom? l) l
    (cons (copy-tree (car l))
          (copy-tree (cdr l)))))

(define (delete-duplicates lst)
  (if (atom? lst)
      lst
      (let ((elt  (car lst))
	    (tail (cdr lst)))
	(if (member elt tail)
	    (delete-duplicates tail)
	    (cons elt
		  (delete-duplicates tail))))))

; backquote -------------------------------------------------------------------

(define (revappend l1 l2) (nconc (reverse  l1) l2))
(define (nreconc   l1 l2) (nconc (reverse! l1) l2))

(define (self-evaluating? x)
  (or (and (atom? x)
           (not (symbol? x)))
      (and (constant? x)
	   (symbol? x)
           (eq x (top-level-value x)))))

(define-macro (backquote x) (bq-process x))

(define (bq-process x)
  (define (splice-form? x)
    (or (and (pair? x) (or (eq (car x) '*comma-at*)
			   (eq (car x) '*comma-dot*)))
	(eq x '*comma*)))
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
             (nconc (cons 'list* forms) (list (bq-process lc))))))
        (#t (let ((p x) (q ()))
	      (while (and (pair? p)
			  (not (eq (car p) '*comma*)))
		     (set! q (cons (bq-bracket (car p)) q))
		     (set! p (cdr p)))
	      (let ((forms
		     (cond ((pair? p) (nreconc q (list (cadr p))))
			   ((null? p)  (reverse! q))
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

; standard macros -------------------------------------------------------------

(define (quote-value v)
  (if (self-evaluating? v)
      v
      (list 'quote v)))

(define-macro (let* binds . body)
  (if (atom? binds) `((lambda () ,@body))
      `((lambda (,(caar binds))
	  ,@(if (pair? (cdr binds))
		`((let* ,(cdr binds) ,@body))
		body))
	,(cadar binds))))

(define-macro (when   c . body) (list 'if c (cons 'begin body) #f))
(define-macro (unless c . body) (list 'if c #f (cons 'begin body)))

(define-macro (case key . clauses)
  (define (vals->cond key v)
    (cond ((eq? v 'else)   'else)
	  ((null? v)       #f)
	  ((symbol? v)     `(eq?  ,key ,(quote-value v)))
          ((atom? v)       `(eqv? ,key ,(quote-value v)))
	  ((null? (cdr v)) `(eqv? ,key ,(quote-value (car v))))
	  ((every symbol? v)
	                   `(memq ,key ',v))
	  (else            `(memv ,key ',v))))
  (let ((g (gensym)))
    `(let ((,g ,key))
       (cond ,.(map (lambda (clause)
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
			     (,loop ,.steps))))))
       (,loop ,.inits))))

; SRFI 8
(define-macro (receive formals expr . body)
  `(call-with-values (lambda () ,expr)
     (lambda ,formals ,@body)))

(define-macro (dotimes var . body)
  (let ((v (car var))
        (cnt (cadr var)))
    `(for 0 (- ,cnt 1)
          (lambda (,v) ,@body))))

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

(define (for-each f l)
  (if (pair? l)
      (begin (f (car l))
	     (for-each f (cdr l)))
      #t))

; exceptions ------------------------------------------------------------------

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
  (let ((e   (gensym))
	(thk (gensym)))
    `(let ((,thk (lambda () ,finally)))
       (prog1 (trycatch ,expr
			(lambda (,e) (begin (,thk) (raise ,e))))
	      (,thk)))))

; debugging utilities ---------------------------------------------------------

(define-macro (assert expr) `(if ,expr #t (raise '(assert-failed ,expr))))

(define traced?
  (letrec ((sample-traced-lambda (lambda args (begin (println (cons 'x args))
						     (apply #.apply args)))))
    (lambda (f)
      (equal? (function:code f)
	      (function:code sample-traced-lambda)))))

(define (trace sym)
  (let* ((func (top-level-value sym))
	 (args (gensym)))
    (if (not (traced? func))
	(set-top-level-value! sym
			      (eval
			       `(lambda ,args
				  (begin (println (cons ',sym ,args))
					 (apply ',func ,args)))))))
  'ok)

(define (untrace sym)
  (let ((func (top-level-value sym)))
    (if (traced? func)
	(set-top-level-value! sym
			      (aref (function:vals func) 2)))))

(define-macro (time expr)
  (let ((t0 (gensym)))
    `(let ((,t0 (time.now)))
       (prog1
	,expr
	(princ "Elapsed time: " (- (time.now) ,t0) " seconds\n")))))

; text I/O --------------------------------------------------------------------

(define (print . args) (apply io.print *output-stream* args))
(define (princ . args) (apply io.princ *output-stream* args))

(define (newline) (princ *linefeed*) #t)
(define (display x) (princ x) #t)
(define (println . args) (prog1 (apply print args) (newline)))

(define (io.readline s) (io.readuntil s #\x0a))

; vector functions ------------------------------------------------------------

(define (list->vector l) (apply vector l))
(define (vector->list v)
  (let ((n (length v))
        (l ()))
    (for 1 n
         (lambda (i)
           (set! l (cons (aref v (- n i)) l))))
    l))

(define (vector.map f v)
  (let* ((n (length v))
         (nv (vector.alloc n)))
    (for 0 (- n 1)
         (lambda (i)
           (aset! nv i (f (aref v i)))))
    nv))

; table functions -------------------------------------------------------------

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
(define (table.invert t)
  (let ((nt (table)))
    (table.foldl (lambda (k v z) (put! nt v k))
		 () t)
    nt))
(define (table.foreach f t)
  (table.foldl (lambda (k v z) (begin (f k v) #t)) () t))

; string functions ------------------------------------------------------------

(define (string.tail s n) (string.sub s (string.inc s 0 n)))

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

(define (string.rep s k)
  (cond ((< k 4)
	 (cond ((<= k 0) "")
	       ((=  k 1) (string s))
	       ((=  k 2) (string s s))
	       (else     (string s s s))))
	((odd? k) (string s (string.rep s (- k 1))))
	(else     (string.rep (string s s) (/ k 2)))))

(define (string.lpad s n c) (string (string.rep c (- n (string.count s))) s))
(define (string.rpad s n c) (string s (string.rep c (- n (string.count s)))))

(define (print-to-string v)
  (let ((b (buffer)))
    (io.print b v)
    (io.tostring! b)))

(define (string.join strlist sep)
  (if (null? strlist) ""
      (let ((b (buffer)))
	(io.write b (car strlist))
	(for-each (lambda (s) (begin (io.write b sep)
				     (io.write b s)))
		  (cdr strlist))
	(io.tostring! b))))

; toplevel --------------------------------------------------------------------

(define (macrocall? e) (and (symbol? (car e))
			    (get *syntax-environment* (car e) #f)))

(define (macroexpand-1 e)
  (if (atom? e) e
      (let ((f (macrocall? e)))
	(if f (apply f (cdr e))
	    e))))

(define (macroexpand e)
  (define (macroexpand-in e env)
    (if (atom? e) e
	(let ((f (assq (car e) env)))
	  (if f
	      (macroexpand-in (apply (cadr f) (cdr e)) (caddr f))
	      (let ((f (macrocall? e)))
		(if f
		    (macroexpand-in (apply f (cdr e)) env)
		    (cond ((eq (car e) 'quote)  e)
			  ((eq (car e) 'lambda)
			   `(lambda ,(cadr e)
			      ,.(map (lambda (x) (macroexpand-in x env))
				     (cddr e))
			      . ,(lastcdr e)))
			  ((eq (car e) 'define)
			   `(define ,(cadr e)
			      ,.(map (lambda (x) (macroexpand-in x env))
				     (cddr e))))
			  ((eq (car e) 'let-syntax)
			   (let ((binds (cadr e))
				 (body  `((lambda () ,@(cddr e)))))
			     (macroexpand-in
			      body
			      (nconc
			       (map (lambda (bind)
				      (list (car bind)
					    (macroexpand-in (cadr bind) env)
					    env))
				    binds)
			       env))))
			  (else
			   (map (lambda (x) (macroexpand-in x env)) e)))))))))
  (macroexpand-in e ()))

(define (expand x) (macroexpand x))

(define (eval x) ((compile-thunk (expand x))))

(define (load-process x) (eval x))

(define (load filename)
  (let ((F (file filename :read)))
    (trycatch
     (let next (prev E v)
       (if (not (io.eof? F))
	   (next (read F)
                 prev
		 (load-process E))
	   (begin (io.close F)
		  ; evaluate last form in almost-tail position
		  (load-process E))))
     (lambda (e)
       (begin
	 (io.close F)
	 (raise `(load-error ,filename ,e)))))))

(define *banner* (string.tail "
;  _
; |_ _ _ |_ _ |  . _ _
; | (-||||_(_)|__|_)|_)
;-------------------|----------------------------------------------------------

" 1))

(define (repl)
  (define (prompt)
    (princ "> ") (io.flush *output-stream*)
    (let ((v (trycatch (read)
		       (lambda (e) (begin (io.discardbuffer *input-stream*)
					  (raise e))))))
      (and (not (io.eof? *input-stream*))
	   (let ((V (load-process v)))
	     (print V)
	     (set! that V)
	     #t))))
  (define (reploop)
    (when (trycatch (and (prompt) (newline))
		    (lambda (e)
		      (print-exception e)
		      (print-stack-trace (stacktrace))
		      #t))
	  (begin (newline)
		 (reploop))))
  (reploop)
  (newline))

(define (print-stack-trace st)
  (define (find-in-f f tgt path)
    (let ((path (cons (function:name f) path)))
      (if (eq? (function:code f) (function:code tgt))
	  (throw 'ffound path)
	  (let ((v (function:vals f)))
	    (for 0 (1- (length v))
		 (lambda (i) (if (closure? (aref v i))
				 (find-in-f (aref v i) tgt path))))))))
  (define (fn-name f e)
    (let ((p (catch 'ffound
		    (begin
		      (for-each (lambda (topfun)
				  (find-in-f topfun f ()))
				e)
		      #f))))
      (if p
	  (symbol (string.join (map string (reverse! p)) "/"))
	  'lambda)))
  (let ((st (reverse! (list-tail st 5)))
	(e (filter closure? (map (lambda (s) (and (bound? s)
						  (top-level-value s)))
				 (environment))))
	(n 0))
    (for-each
     (lambda (f)
       (princ "#" n " ")
       (print (cons (fn-name (aref f 0) e)
		    (cdr (vector->list f))))
       (newline)
       (set! n (+ n 1)))
     st)))

(define (print-exception e)
  (define (eprinc . args) (apply io.princ *error-stream* args))
  (define (eprint . args) (apply io.print *error-stream* args))
  (cond ((and (pair? e)
	      (eq? (car e) 'type-error)
	      (length= e 4))
	 (eprinc "type-error: " (cadr e) ": expected " (caddr e) ", got ")
	 (eprint (cadddr e)))

	((and (pair? e)
	      (eq? (car e) 'unbound-error)
	      (pair? (cdr e)))
	 (eprinc "unbound-error: eval: variable " (cadr e)
		 " has no value"))

	((and (pair? e)
	      (eq? (car e) 'error))
	 (eprinc "error: ")
	 (apply eprinc (cdr e)))

	((and (pair? e)
	      (eq? (car e) 'load-error))
	 (print-exception (caddr e))
	 (eprinc "in file " (cadr e)))

	((and (list? e)
	      (length= e 2))
	 (eprint (car e))
	 (eprinc ": ")
	 (let ((msg (cadr e)))
	   ((if (or (string? msg) (symbol? msg))
		eprinc eprint)
	    msg)))

	(else (eprinc "*** Unhandled exception: ")
	      (eprint e)))

  (eprinc *linefeed*))

(define (simple-sort l)
  (if (or (null? l) (null? (cdr l))) l
      (let* ((piv (car l))
	     (halves (separate (lambda (x) (< x piv)) (cdr l))))
	(nconc (simple-sort (car halves))
	       (list piv)
	       (simple-sort (cdr halves))))))

(define (make-system-image fname)
  (let ((f (file fname :write :create :truncate))
	(excludes '(*linefeed* *directory-separator* *argv* that
		    *print-pretty* *print-width* *print-readably*))
	(pp *print-pretty*))
    (set! *print-pretty* #f)
    (unwind-protect
     (let ((syms (filter (lambda (s)
			   (and (bound? s)
				(not (constant? s))
				(or (not (builtin? (top-level-value s)))
				    (not (equal? (string s) ; alias of builtin
						 (string (top-level-value s)))))
				(not (memq s excludes))
				(not (iostream? (top-level-value s)))))
			 (simple-sort (environment)))))
       (io.print f (apply nconc (map list syms (map top-level-value syms))))
       (io.write f *linefeed*))
     (begin
       (io.close f)
       (set! *print-pretty* pp)))))

; initialize globals that need to be set at load time
(define (__init_globals)
  (if (or (eq? *os-name* 'win32)
	  (eq? *os-name* 'win64)
	  (eq? *os-name* 'windows))
      (begin (set! *directory-separator* "\\")
	     (set! *linefeed* "\r\n"))
      (begin (set! *directory-separator* "/")
	     (set! *linefeed* "\n")))
  (set! *output-stream* *stdout*)
  (set! *input-stream*  *stdin*)
  (set! *error-stream*  *stderr*))

(define (__script fname)
  (trycatch (load fname)
	    (lambda (e) (begin (print-exception e)
			       (exit 1)))))

(define (__start argv)
  (__init_globals)
  (if (pair? (cdr argv))
      (begin (set! *argv* (cdr argv))
	     (__script (cadr argv)))
      (begin (set! *argv* argv)
	     (princ *banner*)
	     (repl)))
  (exit 0))
