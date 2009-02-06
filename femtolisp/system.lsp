; -*- scheme -*-
; femtoLisp standard library
; by Jeff Bezanson (C) 2009
; Distributed under the BSD License

(set-constant! 'eq       eq?)
(set-constant! 'eqv      eqv?)
(set-constant! 'equal    equal?)
(set-constant! 'rplaca   set-car!)
(set-constant! 'rplacd   set-cdr!)
(set-constant! 'char?    (lambda (x) (eq? (typeof x) 'wchar)))

; convert a sequence of body statements to a single expression.
; this allows define, defun, defmacro, let, etc. to contain multiple
; body expressions as in Common Lisp.
(set! f-body (lambda (e)
               (cond ((atom? e)       e)
                     ((eq (cdr e) ()) (car e))
                     (#t              (cons 'begin e)))))

(set-syntax! 'define-macro
             (lambda (form . body)
               (list 'set-syntax! (list 'quote (car form))
                     (list 'lambda (cdr form) (f-body body)))))

(define-macro (label name fn)
  (list (list 'lambda (list name) (list 'set! name fn)) #f))

(define-macro (define form . body)
  (if (symbol? form)
      (list 'set! form (car body))
      (list 'set! (car form) (list 'lambda (cdr form) (f-body body)))))

(define (set s v) (eval (list 'set! s (list 'quote v))))

(define (identity x) x)

(define (map f lst)
  (if (atom? lst) lst
      (cons (f (car lst)) (map f (cdr lst)))))

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
		   (rplacd (last (car lsts))
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
        ((equal      (car lst) item) lst)
        (#t          (member item (cdr lst)))))
(define (memq item lst)
  (cond ((atom? lst) #f)
        ((eq         (car lst) item) lst)
        (#t          (memq item (cdr lst)))))
(define (memv item lst)
  (cond ((atom? lst) #f)
        ((eqv        (car lst) item) lst)
        (#t          (memv item (cdr lst)))))

(define (assoc item lst)
  (cond ((atom? lst) #f)
	((equal      (caar lst) item) (car lst))
	(#t          (assoc item (cdr lst)))))
(define (assv item lst)
  (cond ((atom? lst) #f)
	((eqv        (caar lst) item) (car lst))
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

(define-macro (define form . body)
  (if (symbol? form)
      (list 'set! form (car body))
      (list 'set! (car form)
	    (macroexpand (list 'lambda (cdr form) (f-body body))))))
(define-macro (define-macro form . body)
  (list 'set-syntax! (list 'quote (car form))
	(macroexpand (list 'lambda (cdr form) (f-body body)))))
(define macroexpand (macroexpand macroexpand))

(define =   eqv)
(define eql eqv)
(define (/= a b) (not (equal a b)))
(define != /=)
(define (>  a b) (< b a))
(define (<= a b) (not (< b a)))
(define (>= a b) (not (< a b)))
(define (1+ n) (+ n 1))
(define (1- n) (- n 1))
(define (mod x y) (- x (* (/ x y) y)))
(define remainder mod)
(define (abs x)   (if (< x 0) (- x) x))
(define K prog1)  ; K combinator ;)

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

(define (list* . l)
  (if (atom? (cdr l))
      (car l)
      (cons (car l) (apply list* (cdr l)))))

(define (nlist* . l)
  (if (atom? (cdr l))
      (car l)
      (rplacd l (apply nlist* (cdr l)))))

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
		(rplaca lst (f (car lst)))
		(set! lst (cdr lst)))))

(define (mapcar f . lsts)
  ((label mapcar-
          (lambda (lsts)
            (cond ((null? lsts) (f))
                  ((atom? (car lsts)) (car lsts))
                  (#t (cons (apply f (map car lsts))
			    (mapcar- (map cdr lsts)))))))
   lsts))

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
			  (rplacd l (prog1 prev
					   (set! prev l))))))
    prev))

(define-macro (let* binds . body)
  (cons (list 'lambda (map car binds)
              (cons 'begin
                    (nconc (map (lambda (b) (cons 'set! b)) binds)
                           body)))
        (map (lambda (x) #f) binds)))

(define-macro (labels binds . body)
  (cons (list 'lambda (map car binds)
              (cons 'begin
                    (nconc (map (lambda (b)
                                  (list 'set! (car b) (cons 'lambda (cdr b))))
                                binds)
                           body)))
        (map (lambda (x) #f) binds)))

(define-macro (when   c . body) (list 'if c (f-body body) #f))
(define-macro (unless c . body) (list 'if c #f (f-body body)))

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
             (begin (rplacd acc (cons (f i) ()))
                    (set! acc (cdr acc)))))
      first)))

(define (iota n) (map-int identity n))
(define ι iota)

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

; (try expr
;      (catch (type-error e) . exprs)
;      (catch (io-error e) . exprs)
;      (catch (e) . exprs)
;      (finally . exprs))
(define-macro (try expr . forms)
  (let* ((e        (gensym))
         (reraised (gensym))
         (final (f-body (cdr (or (assq 'finally forms) '(())))))
         (catches (filter (lambda (f) (eq (car f) 'catch)) forms))
         (catchblock `(cond
                       ,.(map (lambda (catc)
                                (let* ((specific (cdr (cadr catc)))
                                       (extype   (caadr catc))
                                       (var      (if specific (car specific)
                                                   extype))
                                       (todo     (cddr catc)))
                                  `(,(if specific
					 ; exception matching logic
                                         `(or (eq ,e ',extype)
                                              (and (pair? ,e)
                                                   (eq (car ,e)
                                                       ',extype)))
					 #t); (catch (e) ...), match anything
                                    (let ((,var ,e)) (begin ,@todo)))))
                              catches)
                       (#t (raise ,e))))) ; no matches, reraise
    (if final
        (if catches
            ; form with both catch and finally
            `(prog1 (trycatch ,expr
                              (lambda (,e)
                                (trycatch ,catchblock
                                          (lambda (,reraised)
                                            (begin ,final
                                                   (raise ,reraised))))))
               ,final)
          ; finally only; same as unwind-protect
          `(prog1 (trycatch ,expr (lambda (,e)
                                    (begin ,final (raise ,e))))
             ,final))
      ; catch, no finally
      `(trycatch ,expr (lambda (,e) ,catchblock)))))

; setf
; expands (setf (place x ...) v) to (mutator (f x ...) v)
; (mutator (identity x ...) v) is interpreted as (mutator x ... v)
(set! *setf-place-list*
       ; place   mutator  f
      '((car     rplaca   identity)
        (cdr     rplacd   identity)
        (caar    rplaca   car)
        (cadr    rplaca   cdr)
        (cdar    rplacd   car)
        (cddr    rplacd   cdr)
        (caaar   rplaca   caar)
        (caadr   rplaca   cadr)
        (cadar   rplaca   cdar)
        (caddr   rplaca   cddr)
        (cdaar   rplacd   caar)
        (cdadr   rplacd   cadr)
        (cddar   rplacd   cdar)
        (cdddr   rplacd   cddr)
        (list-ref rplaca  nthcdr)
        (get     put!     identity)
        (aref    aset!    identity)
        (symbol-syntax    set-syntax!        identity)))

(define (setf-place-mutator place val)
  (if (symbol? place)
      (list 'set! place val)
    (let ((mutator (assq (car place) *setf-place-list*)))
      (if (null? mutator)
          (error "setf: unknown place " (car place))
	  (if (eq (caddr mutator) 'identity)
	      (cons (cadr mutator) (append (cdr place) (list val)))
	      (list (cadr mutator)
		    (cons (caddr mutator) (cdr place))
		    val))))))

(define-macro (setf . args)
  (f-body
   ((label setf-
           (lambda (args)
             (if (null? args)
                 ()
               (cons (setf-place-mutator (car args) (cadr args))
                     (setf- (cddr args))))))
    args)))

(define (revappend l1 l2) (nconc (reverse l1) l2))
(define (nreconc   l1 l2) (nconc (nreverse l1) l2))

(define (list-to-vector l) (apply vector l))
(define (vector-to-list v)
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
             (let ((body (bq-process (vector-to-list x))))
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

(define-macro (assert expr) `(if ,expr #t (raise '(assert-failed ,expr))))

(define-macro (time expr)
  (let ((t0 (gensym)))
    `(let ((,t0 (time.now)))
       (prog1
	,expr
	(princ "Elapsed time: " (- (time.now) ,t0) " seconds\n")))))

(define (display x) (princ x) #t)

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

(define *whitespace*
  (string.encode #array(wchar 9 10 11 12 13 32 133 160 5760 6158 8192
			      8193 8194 8195 8196 8197 8198 8199 8200
			      8201 8202 8232 8233 8239 8287 12288)))
