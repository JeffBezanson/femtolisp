; -*- scheme -*-

(define Instructions
  (let ((e (table))
	(keys 
	 [:nop :dup :pop :call :tcall :jmp :brf :brt :jmp.l :brf.l :brt.l :ret
	  
	  :eq? :eqv? :equal? :atom? :not :null? :boolean? :symbol?
	  :number? :bound? :pair? :builtin? :vector? :fixnum? :function?
	  
	  :cons :list :car :cdr :set-car! :set-cdr!
	  :apply
	  
	  :+ :- :* :/ :div0 := :< :compare
	  
	  :vector :aref :aset!
	  
	  :loadt :loadf :loadnil :load0 :load1 :loadi8
	  :loadv :loadv.l
	  :loadg :loadg.l
	  :loada :loada.l :loadc :loadc.l
	  :setg :setg.l
	  :seta :seta.l :setc :setc.l
	  
	  :closure :argc :vargc :trycatch :copyenv :let :for :tapply
	  :add2 :sub2 :neg :largc :lvargc
	  :loada0 :loada1 :loadc00 :loadc01
	  
	  dummy_t dummy_f dummy_nil]))
    (for 0 (1- (length keys))
	 (lambda (i)
	   (put! e (aref keys i) i)))))

(define arg-counts
  (table :eq?      2      :eqv?     2
	 :equal?   2      :atom?    1
	 :not      1      :null?    1
	 :boolean? 1      :symbol?  1
	 :number?  1      :bound?   1
	 :pair?    1      :builtin? 1
	 :vector?  1      :fixnum?  1
	 :cons     2      :car      1
	 :cdr      1      :set-car! 2
	 :set-cdr! 2      :=        2
         :<        2      :compare  2
         :aref     2      :aset!    3
	 :div0     2))

(define (make-code-emitter) (vector () (table) 0 +inf.0))
(define (bcode:code   b) (aref b 0))
(define (bcode:ctable b) (aref b 1))
(define (bcode:nconst b) (aref b 2))
(define (bcode:cdepth b d) (aset! b 3 (min (aref b 3) d)))
; get an index for a referenced value in a bytecode object
(define (bcode:indexfor b v)
  (let ((const-to-idx (bcode:ctable b))
	(nconst       (bcode:nconst b)))
    (if (has? const-to-idx v)
	(get const-to-idx v)
	(begin (put! const-to-idx v nconst)
	       (prog1 nconst
		      (aset! b 2 (+ nconst 1)))))))
(define (emit e inst . args)
  (if (null? args)
      (aset! e 0 (cons inst (aref e 0)))
      (begin
	(if (memq inst '(:loadv :loadg :setg))
	    (set! args (list (bcode:indexfor e (car args)))))
	(let ((longform
	       (assq inst '((:loadv :loadv.l) (:loadg :loadg.l) (:setg :setg.l)
			    (:loada :loada.l) (:seta  :seta.l)))))
	  (if (and longform
		   (> (car args) 255))
	      (set! inst (cadr longform))))
	(let ((longform
	       (assq inst '((:loadc :loadc.l) (:setc :setc.l)))))
	  (if (and longform
		   (or (> (car  args) 255)
		       (> (cadr args) 255)))
	      (set! inst (cadr longform))))
	(if (eq? inst :loada)
	    (cond ((equal? args '(0))
		   (set! inst :loada0)
		   (set! args ()))
		  ((equal? args '(1))
		   (set! inst :loada1)
		   (set! args ()))))
	(if (eq? inst :loadc)
	    (cond ((equal? args '(0 0))
		   (set! inst :loadc00)
		   (set! args ()))
		  ((equal? args '(0 1))
		   (set! inst :loadc01)
		   (set! args ()))))
	(aset! e 0 (nreconc (cons inst args) (aref e 0)))))
  e)

(define (make-label e)   (gensym))
(define (mark-label e l) (emit e :label l))

; convert symbolic bytecode representation to a byte array.
; labels are fixed-up.
(define (encode-byte-code e)
  (let* ((cl (reverse! e))
	 (v  (list->vector cl))
	 (long? (>= (+ (length v)  ; 1 byte for each entry, plus...
		       ; at most half the entries in this vector can be
		       ; instructions accepting 32-bit arguments
		       (* 3 (div0 (length v) 2))
		       #;(* 3 (count (lambda (i)
				     (memq i '(:loadv.l :loadg.l :setg.l
					       :loada.l :seta.l :loadc.l
					       :setc.l :jmp :brt :brf
					       :largc :lvargc)))
				   cl)))
		    65536)))
    (let ((n              (length v))
	  (i              0)
	  (label-to-loc   (table))
	  (fixup-to-label (table))
	  (bcode          (buffer))
	  (vi             #f)
	  (nxt            #f))
      (io.write bcode #int32(0))
      (while (< i n)
	(begin
	  (set! vi (aref v i))
	  (if (eq? vi :label)
	      (begin (put! label-to-loc (aref v (+ i 1)) (sizeof bcode))
		     (set! i (+ i 2)))
	      (begin
		(io.write bcode
			  (byte
			   (get Instructions
				(if long?
				    (case vi
				      (:jmp :jmp.l)
				      (:brt :brt.l)
				      (:brf :brf.l)
				      (else vi))
				    vi))))
		(set! i (+ i 1))
		(set! nxt (if (< i n) (aref v i) #f))
		(cond ((memq vi '(:jmp :brf :brt))
		       (put! fixup-to-label (sizeof bcode) nxt)
		       (io.write bcode ((if long? int32 int16) 0))
		       (set! i (+ i 1)))
		      ((number? nxt)
		       (case vi
			 ((:loadv.l :loadg.l :setg.l :loada.l :seta.l
			   :largc :lvargc)
			  (io.write bcode (int32 nxt))
			  (set! i (+ i 1)))
			 
			 ((:loadc :setc)  ; 2 uint8 args
			  (io.write bcode (uint8 nxt))
			  (set! i (+ i 1))
			  (io.write bcode (uint8 (aref v i)))
			  (set! i (+ i 1)))
			 
			 ((:loadc.l :setc.l)  ; 2 int32 args
			  (io.write bcode (int32 nxt))
			  (set! i (+ i 1))
			  (io.write bcode (int32 (aref v i)))
			  (set! i (+ i 1)))
			 
			 (else
			  ; other number arguments are always uint8
			  (io.write bcode (uint8 nxt))
			  (set! i (+ i 1)))))
		      (else #f))))))

      (table.foreach
       (lambda (addr labl)
	 (begin (io.seek bcode addr)
		(io.write bcode ((if long? int32 int16)
				 (- (get label-to-loc labl)
				    addr)))))
       fixup-to-label)
      (io.tostring! bcode))))

(define (const-to-idx-vec e)
  (let ((cvec (vector.alloc (bcode:nconst e))))
    (table.foreach (lambda (val idx) (aset! cvec idx val))
		   (bcode:ctable e))
    cvec))

(define (index-of item lst start)
  (cond ((null? lst) #f)
	((eq? item (car lst)) start)
	(else (index-of item (cdr lst) (+ start 1)))))

(define (in-env? s env) (any (lambda (e) (memq s e)) env))

(define (lookup-sym s env lev arg?)
  (if (null? env)
      '(global)
      (let* ((curr (car env))
	     (i    (index-of s curr 0)))
	(if i
	    (if arg?
		`(arg ,i)
		`(closed ,lev ,i))
	    (lookup-sym s
			(cdr env)
			(if (or arg? (null? curr)) lev (+ lev 1))
			#f)))))

; number of non-nulls
(define (nnn e) (count (lambda (x) (not (null? x))) e))

(define (compile-sym g env s Is)
  (let ((loc (lookup-sym s env 0 #t)))
    (case (car loc)
      (arg     (emit g (aref Is 0) (cadr loc)))
      (closed  (emit g (aref Is 1) (cadr loc) (caddr loc))
	       ; update index of most distant captured frame
	       (bcode:cdepth g (- (nnn (cdr env)) 1 (cadr loc))))
      (else    (emit g (aref Is 2) s)))))

(define (compile-if g env tail? x)
  (let ((elsel (make-label g))
	(endl  (make-label g))
	(test  (cadr x))
	(then  (caddr x))
	(else  (if (pair? (cdddr x))
		   (cadddr x)
		   #f)))
    (cond ((eq? test #t)
	   (compile-in g env tail? then))
	  ((eq? test #f)
	   (compile-in g env tail? else))
	  (else
	   (compile-in g env #f test)
	   (emit g :brf elsel)
	   (compile-in g env tail? then)
	   (if tail?
	       (emit g :ret)
	       (emit g :jmp endl))
	   (mark-label g elsel)
	   (compile-in g env tail? else)
	   (mark-label g endl)))))

(define (compile-begin g env tail? forms)
  (cond ((atom? forms) (compile-in g env tail? #f))
	((atom? (cdr forms))
	 (compile-in g env tail? (car forms)))
	(else
	 (compile-in g env #f (car forms))
	 (emit g :pop)
	 (compile-begin g env tail? (cdr forms)))))

(define (compile-prog1 g env x)
  (compile-in g env #f (cadr x))
  (if (pair? (cddr x))
      (begin (compile-begin g env #f (cddr x))
	     (emit g :pop))))

(define (compile-while g env cond body)
  (let ((top  (make-label g))
	(end  (make-label g)))
    (compile-in g env #f #f)
    (mark-label g top)
    (compile-in g env #f cond)
    (emit g :brf end)
    (emit g :pop)
    (compile-in g env #f body)
    (emit g :jmp top)
    (mark-label g end)))

(define (1arg-lambda? func)
  (and (pair? func)
       (eq? (car func) 'lambda)
       (pair? (cdr func))
       (pair? (cadr func))
       (length= (cadr func) 1)))

(define (compile-for g env lo hi func)
  (if (1arg-lambda? func)
      (begin (compile-in g env #f lo)
	     (compile-in g env #f hi)
	     (compile-in g env #f func)
	     (emit g :for))
      (error "for: third form must be a 1-argument lambda")))

(define (compile-short-circuit g env tail? forms default branch)
  (cond ((atom? forms)        (compile-in g env tail? default))
	((atom? (cdr forms))  (compile-in g env tail? (car forms)))
	(else
	 (let ((end  (make-label g)))
	   (compile-in g env #f (car forms))
	   (emit g :dup)
	   (emit g branch end)
	   (emit g :pop)
	   (compile-short-circuit g env tail? (cdr forms) default branch)
	   (mark-label g end)))))

(define (compile-and g env tail? forms)
  (compile-short-circuit g env tail? forms #t :brf))
(define (compile-or g env tail? forms)
  (compile-short-circuit g env tail? forms #f :brt))

(define MAX_ARGS 127)

(define (list-partition l n)
  (define (list-part- l n  i subl acc)
    (cond ((atom? l) (if (> i 0)
			 (cons (reverse! subl) acc)
			 acc))
	  ((>= i n)  (list-part- l n 0 () (cons (reverse! subl) acc)))
	  (else      (list-part- (cdr l) n (+ 1 i) (cons (car l) subl) acc))))
  (if (<= n 0)
      (error "list-partition: invalid count")
      (reverse! (list-part- l n 0 () ()))))

(define (just-compile-args g lst env)
  (for-each (lambda (a)
	      (compile-in g env #f a))
	    lst))

(define (compile-arglist g env lst)
  (let ((argtail (length> lst MAX_ARGS)))
    (if argtail
	(begin (just-compile-args g (list-head lst MAX_ARGS) env)
	       (let ((rest
		      (cons nconc
			    (map (lambda (l) (cons list l))
				 (list-partition argtail MAX_ARGS)))))
		 (compile-in g env #f rest))
	       (+ MAX_ARGS 1))
	(begin (just-compile-args g lst env)
	       (length lst)))))

(define (argc-error head count)
  (error (string "compile error: " head " expects " count
		 (if (= count 1)
		     " argument."
		     " arguments."))))

(define (compile-app g env tail? x)
  (let ((head (car x)))
    (if (and (pair? head)
	     (eq? (car head) 'lambda)
	     (list? (cadr head))
	     (not (length> (cadr head) MAX_ARGS)))
	(compile-let  g env tail? x)
	(compile-call g env tail? x))))

(define (compile-let g env tail? x)
  (let ((head (car x))
	(args (cdr x)))
    (unless (length= args (length (cadr head)))
	    (error (string "apply: incorrect number of arguments to " head)))
    (receive (the-f dept) (compile-f- env head #t)
      (emit g :loadv the-f)
      (bcode:cdepth g dept))
    (let ((nargs (compile-arglist g env args)))
      (emit g :copyenv)
      (emit g (if tail? :tcall :call) (+ 1 nargs)))))

(define builtin->instruction
  (let ((b2i (table number? :number?  cons :cons
		    fixnum? :fixnum?  equal? :equal?
		    eq? :eq?  symbol? :symbol?
		    div0 :div0  builtin? :builtin?
		    aset! :aset!  - :-  boolean? :boolean?  not :not
		    apply :apply  atom? :atom?
		    set-cdr! :set-cdr!  / :/
		    function? :function?  vector :vector
		    list :list  bound? :bound?
		    < :<  * :* cdr :cdr  null? :null?
		    + :+  eqv? :eqv? compare :compare  aref :aref
		    set-car! :set-car!  car :car
		    pair? :pair?  = :=  vector? :vector?)))
    (lambda (b)
      (get b2i b #f))))

(define (compile-call g env tail? x)
  (let ((head  (car x)))
    (let ((head
	   (if (and (symbol? head)
		    (not (in-env? head env))
		    (bound? head)
		    (constant? head)
		    (builtin? (top-level-value head)))
	       (top-level-value head)
	       head)))
      (let ((b (and (builtin? head)
		    (builtin->instruction head))))
	(if (not b)
	    (compile-in g env #f head))
	(let ((nargs (compile-arglist g env (cdr x))))
	  (if b
	      (let ((count (get arg-counts b #f)))
		(if (and count
			 (not (length= (cdr x) count)))
		    (argc-error head count))
		(case b  ; handle special cases of vararg builtins
		  (:list (if (= nargs 0) (emit g :loadnil) (emit g b nargs)))
		  (:+    (cond ((= nargs 0) (emit g :load0))
			       ((= nargs 2) (emit g :add2))
			       (else (emit g b nargs))))
		  (:-    (cond ((= nargs 0) (argc-error head 1))
			       ((= nargs 1) (emit g :neg))
			       ((= nargs 2) (emit g :sub2))
			       (else (emit g b nargs))))
		  (:*    (if (= nargs 0) (emit g :load1)
			     (emit g b nargs)))
		  (:/    (if (= nargs 0)
			     (argc-error head 1)
			     (emit g b nargs)))
		  (:vector   (if (= nargs 0)
				 (emit g :loadv [])
				 (emit g b nargs)))
		  (:apply    (if (< nargs 2)
				 (argc-error head 2)
				 (emit g (if tail? :tapply :apply) nargs)))
		  (else      (emit g b))))
	      (emit g (if tail? :tcall :call) nargs)))))))

(define (expand-define form body)
  (if (symbol? form)
      `(set! ,form ,(car body))
      `(set! ,(car form)
	     (lambda ,(cdr form) ,@body . ,(car form)))))

(define (fits-i8 x) (and (fixnum? x) (>= x -128) (<= x 127)))

(define (compile-in g env tail? x)
  (cond ((symbol? x) (compile-sym g env x [:loada :loadc :loadg]))
	((atom? x)
	 (cond ((eq? x 0)   (emit g :load0))
	       ((eq? x 1)   (emit g :load1))
	       ((eq? x #t)  (emit g :loadt))
	       ((eq? x #f)  (emit g :loadf))
	       ((eq? x ())  (emit g :loadnil))
	       ((fits-i8 x) (emit g :loadi8 x))
	       (else        (emit g :loadv x))))
	(else
	 (case (car x)
	   (quote    (emit g :loadv (cadr x)))
	   (if       (compile-if g env tail? x))
	   (begin    (compile-begin g env tail? (cdr x)))
	   (prog1    (compile-prog1 g env x))
	   (lambda   (receive (the-f dept) (compile-f- env x)
		       (begin (emit g :loadv the-f)
			      (bcode:cdepth g dept)
			      (if (< dept (nnn env))
				  (emit g :closure)))))
	   (and      (compile-and g env tail? (cdr x)))
	   (or       (compile-or  g env tail? (cdr x)))
	   (while    (compile-while g env (cadr x) (cons 'begin (cddr x))))
	   (for      (compile-for   g env (cadr x) (caddr x) (cadddr x)))
	   (return   (compile-in g env #t (cadr x))
		     (emit g :ret))
	   (set!     (compile-in g env #f (caddr x))
		     (compile-sym g env (cadr x) [:seta :setc :setg]))
	   (define   (compile-in g env tail?
				 (expand-define (cadr x) (cddr x))))
	   (trycatch (compile-in g env #f `(lambda () ,(cadr x)))
		     (unless (1arg-lambda? (caddr x))
			     (error "trycatch: second form must be a 1-argument lambda"))
		     (compile-in g env #f (caddr x))
		     (emit g :trycatch))
	   (else   (compile-app g env tail? x))))))

(define (compile-f env f . let?)
  (receive (ff ignore)
	   (apply compile-f- env f let?)
	   ff))

(define get-defined-vars
  (letrec ((get-defined-vars-
	    (lambda (expr)
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
		    (else ())))))
    (lambda (expr) (delete-duplicates (get-defined-vars- expr)))))

(define compile-f-
  (let ((*defines-processed-token* (gensym)))
    ; to eval a top-level expression we need to avoid internal define
    (set-top-level-value!
     'compile-thunk
     (lambda (expr)
       (compile `(lambda () ,expr . ,*defines-processed-token*))))

    (lambda (env f . let?)
      ; convert lambda to one body expression and process internal defines
      (define (lambda-body e)
	(let ((B (if (pair? (cddr e))
		     (if (pair? (cdddr e))
			 (cons 'begin (cddr e))
			 (caddr e))
		     #f)))
	  (let ((V (get-defined-vars B)))
	    (if (null? V)
		B
		(cons (list* 'lambda V B *defines-processed-token*)
		      (map (lambda (x) #f) V))))))
      
      (let ((g    (make-code-emitter))
	    (args (cadr f))
	    (name (if (eq? (lastcdr f) *defines-processed-token*)
		      'lambda
		      (lastcdr f))))
	(cond ((not (null? let?))      (emit g :let))
	      ((length> args MAX_ARGS) (emit g (if (null? (lastcdr args))
						   :largc :lvargc)
					     (length args)))
	      ((null? (lastcdr args))  (emit g :argc  (length args)))
	      (else  (emit g :vargc (if (atom? args) 0 (length args)))))
	(compile-in g (cons (to-proper args) env) #t
		    (if (eq? (lastcdr f) *defines-processed-token*)
			(caddr f)
			(lambda-body f)))
	(emit g :ret)
	(values (function (encode-byte-code (bcode:code g))
			  (const-to-idx-vec g) name)
		(aref g 3))))))

(define (compile f) (compile-f () f))

(define (ref-int32-LE a i)
  (int32 (+ (ash (aref a (+ i 0)) 0)
	    (ash (aref a (+ i 1)) 8)
	    (ash (aref a (+ i 2)) 16)
	    (ash (aref a (+ i 3)) 24))))

(define (ref-int16-LE a i)
  (int16 (+ (ash (aref a (+ i 0)) 0)
	    (ash (aref a (+ i 1)) 8))))

(define (hex5 n)
  (string.lpad (number->string n 16) 5 #\0))

(define (disassemble f . lev?)
  (if (null? lev?)
      (begin (disassemble f 0)
	     (newline)
	     (return #t)))
  (let ((lev (car lev?))
	(code (function:code f))
	(vals (function:vals f)))
    (define (print-val v)
      (if (and (function? v) (not (builtin? v)))
	  (begin (princ "\n")
		 (disassemble v (+ lev 1)))
	  (print v)))
    (dotimes (xx lev) (princ "\t"))
    (princ "maxstack " (ref-int32-LE code 0) "\n")
    (let ((i 4)
	  (N (length code)))
      (while (< i N)
	     ; find key whose value matches the current byte
	     (let ((inst (table.foldl (lambda (k v z)
					(or z (and (eq? v (aref code i))
						   k)))
				      #f Instructions)))
	       (if (> i 4) (newline))
	       (dotimes (xx lev) (princ "\t"))
	       (princ (hex5 (- i 4)) ":  "
		      (string.tail (string inst) 1) "\t")
	       (set! i (+ i 1))
	       (case inst
		 ((:loadv.l :loadg.l :setg.l)
		  (print-val (aref vals (ref-int32-LE code i)))
		  (set! i (+ i 4)))
		 
		 ((:loadv :loadg :setg)
		  (print-val (aref vals (aref code i)))
		  (set! i (+ i 1)))
		 
		 ((:loada :seta :call :tcall :list :+ :- :* :/ :vector
		   :argc :vargc :loadi8 :apply :tapply)
		  (princ (number->string (aref code i)))
		  (set! i (+ i 1)))
		 
		 ((:loada.l :seta.l :largc :lvargc)
		  (princ (number->string (ref-int32-LE code i)))
		  (set! i (+ i 4)))

		 ((:loadc :setc)
		  (princ (number->string (aref code i)) " ")
		  (set! i (+ i 1))
		  (princ (number->string (aref code i)))
		  (set! i (+ i 1)))
		 
		 ((:loadc.l :setc.l)
		  (princ (number->string (ref-int32-LE code i)) " ")
		  (set! i (+ i 4))
		  (princ (number->string (ref-int32-LE code i)))
		  (set! i (+ i 4)))
		 
		 ((:jmp :brf :brt)
		  (princ "@" (hex5 (+ i -4 (ref-int16-LE code i))))
		  (set! i (+ i 2)))
		 
		 ((:jmp.l :brf.l :brt.l)
		  (princ "@" (hex5 (+ i -4 (ref-int32-LE code i))))
		  (set! i (+ i 4)))
		 
		 (else #f)))))))

#t
