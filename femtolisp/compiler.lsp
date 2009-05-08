; -*- scheme -*-

(define (make-enum-table offset keys)
  (let ((e (table)))
    (for 0 (1- (length keys))
	 (lambda (i)
	   (put! e (aref keys i) (+ offset i))))))

(define Instructions
  (make-enum-table 0
   [:nop :dup :pop :call :tcall :jmp :brf :brt :jmp.l :brf.l :brt.l :ret

    :eq? :eqv? :equal? :atom? :not :null? :boolean? :symbol?
    :number? :bound? :pair? :builtin? :vector? :fixnum? :function?

    :cons :list :car :cdr :set-car! :set-cdr!
    :apply

    :+ :- :* :/ := :< :compare

    :vector :aref :aset!

    :loadt :loadf :loadnil :load0 :load1 :loadi8 :loadv :loadv.l
    :loadg :loada :loadc :loadg.l
    :setg  :seta  :setc  :setg.l

    :closure :trycatch :argc :vargc :copyenv :let :for :tapply :add2 :sub2 :neg

    dummy_t dummy_f dummy_nil]))

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
	 :set-cdr! 2      :apply    2
         :<        2      :compare  2
         :aref     2      :aset!    3
	 :=        2))

(define (make-code-emitter) (vector () (table) 0))
(define (emit e inst . args)
  (if (memq inst '(:loadv :loadg :setg))
      (let* ((const-to-idx (aref e 1))
	     (nconst       (aref e 2))
	     (v            (car args))
	     (vind (if (has? const-to-idx v)
		       (get const-to-idx v)
		       (begin (put! const-to-idx v nconst)
			      (set! nconst (+ nconst 1))
			      (- nconst 1)))))
	(aset! e 2 nconst)
	(set! args (list vind))
	(if (>= vind 256)
	    (set! inst (case inst
			 (:loadv :loadv.l)
			 (:loadg :loadg.l)
			 (:setg  :setg.l))))))
  (aset! e 0 (nreconc (cons inst args) (aref e 0)))
  e)

(define (make-label e)   (gensym))
(define (mark-label e l) (emit e :label l))

(define (count f l)
  (define (count- f l n)
    (if (null? l)
	n
	(count- f (cdr l) (if (f (car l))
			      (+ n 1)
			      n))))
  (count- f l 0))

(define (peephole c) c)

; convert symbolic bytecode representation to a byte array.
; labels are fixed-up.
(define (encode-byte-code e)
  (let* ((cl (peephole (nreverse e)))
	 (long? (>= (+ (length cl)
		       (* 3 (count (lambda (i)
				     (memq i '(:loadv :loadg :setg
						      :jmp :brt :brf)))
				   cl)))
		    65536))
	 (v  (list->vector cl)))
    (let ((n              (length v))
	  (i              0)
	  (label-to-loc   (table))
	  (fixup-to-label (table))
	  (bcode          (buffer))
	  (vi             #f))
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
				(if (and long?
					 (memq vi '(:jmp :brt :brf)))
				    (case vi
				      (:jmp :jmp.l)
				      (:brt :brt.l)
				      (:brf :brf.l))
				    vi))))
		(set! i (+ i 1))
		(if (< i n)
		    (let ((nxt (aref v i)))
		      (case vi
			((:loadv.l :loadg.l :setg.l)
			 (io.write bcode (uint32 nxt))
			 (set! i (+ i 1)))
			
			((:loada :seta :call :tcall :loadv :loadg :setg
			  :list :+ :- :* :/ :vector :argc :vargc :loadi8)
			 (io.write bcode (uint8 nxt))
			 (set! i (+ i 1)))
			
			((:loadc :setc)  ; 2 uint8 args
			 (io.write bcode (uint8 nxt))
			 (set! i (+ i 1))
			 (io.write bcode (uint8 (aref v i)))
			 (set! i (+ i 1)))
			
			((:jmp :brf :brt)
			 (put! fixup-to-label (sizeof bcode) nxt)
			 (io.write bcode ((if long? uint32 uint16) 0))
			 (set! i (+ i 1)))
			
			(else #f))))))))
      (table.foreach
       (lambda (addr labl)
	 (begin (io.seek bcode addr)
		(io.write bcode ((if long? uint32 uint16)
				 (get label-to-loc labl)))))
       fixup-to-label)
      (io.tostring! bcode))))

(define (const-to-idx-vec e)
  (let ((cvec (vector.alloc (aref e 2))))
    (table.foreach (lambda (val idx) (aset! cvec idx val))
		   (aref e 1))
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

(define (compile-sym g env s Is)
  (let ((loc (lookup-sym s env 0 #t)))
    (case (car loc)
      (arg     (emit g (aref Is 0) (cadr loc)))
      (closed  (emit g (aref Is 1) (cadr loc) (caddr loc)))
      (else    (emit g (aref Is 2) s)))))

(define (cond->if form)
  (cond-clauses->if (cdr form)))
(define (cond-clauses->if lst)
  (if (atom? lst)
      #f
      (let ((clause (car lst)))
	(if (or (eq? (car clause) 'else)
		(eq? (car clause) #t))
	    (cons 'begin (cdr clause))
	    `(if ,(car clause)
		 ,(cons 'begin (cdr clause))
		 ,(cond-clauses->if (cdr lst)))))))

(define (compile-if g env tail? x)
  (let ((elsel (make-label g))
	(endl  (make-label g)))
    (compile-in g env #f (cadr x))
    (emit g :brf elsel)
    (compile-in g env tail? (caddr x))
    (if tail?
	(emit g :ret)
	(emit g :jmp endl))
    (mark-label g elsel)
    (compile-in g env tail?
		(if (pair? (cdddr x))
		    (cadddr x)
		    #f))
    (mark-label g endl)))

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
			 (cons (nreverse subl) acc)
			 acc))
	  ((>= i n)  (list-part- l n 0 () (cons (nreverse subl) acc)))
	  (else      (list-part- (cdr l) n (+ 1 i) (cons (car l) subl) acc))))
  (if (<= n 0)
      (error "list-partition: invalid count")
      (nreverse (list-part- l n 0 () ()))))

(define (length> lst n)
  (cond ((< n 0)     lst)
	((= n 0)     (and (pair? lst) lst))
	((null? lst) (< n 0))
	(else        (length> (cdr lst) (- n 1)))))

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

(define (emit-nothing g) g)

(define (argc-error head count)
  (error (string "compile error: " head " expects " count
		 (if (= count 1)
		     " argument."
		     " arguments."))))

(define (compile-app g env tail? x)
  (let ((head (car x)))
    (if (and (pair? head)
	     (eq? (car head) 'lambda)
	     (list? (cadr head)))
	(compile-let  g env tail? x)
	(compile-call g env tail? x))))

(define (compile-let g env tail? x)
  (let ((head (car x))
	(args (cdr x)))
    (unless (length= args (length (cadr head)))
	    (error (string "apply: incorrect number of arguments to " head)))
    (emit g :loadv (compile-f env head #t))
    (let ((nargs (compile-arglist g env args)))
      (emit g :copyenv)
      (emit g (if tail? :tcall :call) (+ 1 nargs)))))

(define (builtin->instruction b)
  (let ((sym (intern (string #\: b))))
    (and (has? Instructions sym) sym)))

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
	(if (eq? b :apply)
	    (cond ((length= x 4)
		   (set! x `(,head ,(cadr x) (cons ,@(cddr x)))))
		  ((length> x 4)
		   (set! x `(,head ,(cadr x)
				   (nconc (list ,@(list-head (cddr x)
							     (- (length x) 3)))
					  ,(car (last-pair x))))))))
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
		  (else
		   (emit g (if (and tail? (eq? b :apply)) :tapply b)))))
	      (emit g (if tail? :tcall :call) nargs)))))))

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
	   (cond     (compile-in g env tail? (cond->if x)))
	   (if       (compile-if g env tail? x))
	   (begin    (compile-begin g env tail? (cdr x)))
	   (prog1    (compile-prog1 g env x))
	   (lambda   (begin (emit g :loadv (compile-f env x))
			    (emit g :closure)))
	   (and      (compile-and g env tail? (cdr x)))
	   (or       (compile-or  g env tail? (cdr x)))
	   (while    (compile-while g env (cadr x) (cons 'begin (cddr x))))
	   (for      (compile-for   g env (cadr x) (caddr x) (cadddr x)))
	   (return   (compile-in g env #t (cadr x))
		     (emit g :ret))
	   (set!     (compile-in g env #f (caddr x))
		     (compile-sym g env (cadr x) [:seta :setc :setg]))
	   (trycatch (compile-in g env #f `(lambda () ,(cadr x)))
		     (unless (1arg-lambda? (caddr x))
			     (error "trycatch: second form must be a 1-argument lambda"))
		     (compile-in g env #f (caddr x))
		     (emit g :trycatch))
	   (else   (compile-app g env tail? x))))))

(define (compile-f env f . let?)
  (let ((g    (make-code-emitter))
	(args (cadr f)))
    (cond ((not (null? let?))     (emit g :let))
	  ((null? (lastcdr args)) (emit g :argc (length args)))
	  (else  (emit g :vargc (if (atom? args) 0 (length args)))))
    (compile-in g (cons (to-proper args) env) #t (caddr f))
    (emit g :ret)
    (function (encode-byte-code (aref g 0))
	      (const-to-idx-vec g))))

(define (compile f) (compile-f () f))

(define (compile-thunk expr) (compile `(lambda () ,expr)))

(define (ref-uint32-LE a i)
  (+ (ash (aref a (+ i 0)) 0)
     (ash (aref a (+ i 1)) 8)
     (ash (aref a (+ i 2)) 16)
     (ash (aref a (+ i 3)) 24)))

(define (ref-uint16-LE a i)
  (+ (ash (aref a (+ i 0)) 0)
     (ash (aref a (+ i 1)) 8)))

(define (hex5 n)
  (string.lpad (number->string n 16) 5 #\0))

(define (disassemble f . lev?)
  (if (null? lev?)
      (begin (disassemble f 0)
	     (newline)
	     (return #t)))
  (let ((fvec (function->vector f))
	(lev (car lev?)))
    (let ((code (aref fvec 0))
	  (vals (aref fvec 1)))
      (define (print-val v)
	(if (and (function? v) (not (builtin? v)))
	    (begin (princ "\n")
		   (disassemble v (+ lev 1)))
	    (print v)))
      (let ((i 0)
	    (N (length code)))
	(while (< i N)
	       ; find key whose value matches the current byte
	       (let ((inst (table.foldl (lambda (k v z)
					  (or z (and (eq? v (aref code i))
						     k)))
					#f Instructions)))
		 (if (> i 0) (newline))
		 (dotimes (xx lev) (princ "\t"))
		 (princ (hex5 i) ":  "
			(string.tail (string inst) 1) "\t")
		 (set! i (+ i 1))
		 (case inst
		   ((:loadv.l :loadg.l :setg.l)
		    (print-val (aref vals (ref-uint32-LE code i)))
		    (set! i (+ i 4)))
		   
		   ((:loadv :loadg :setg)
		    (print-val (aref vals (aref code i)))
		    (set! i (+ i 1)))
		   
		   ((:loada :seta :call :tcall :list :+ :- :* :/ :vector
			    :argc :vargc :loadi8)
		    (princ (number->string (aref code i)))
		    (set! i (+ i 1)))
		   
		   ((:loadc :setc)
		    (princ (number->string (aref code i)) " ")
		    (set! i (+ i 1))
		    (princ (number->string (aref code i)))
		    (set! i (+ i 1)))
		   
		   ((:jmp :brf :brt)
		    (princ "@" (hex5 (ref-uint16-LE code i)))
		    (set! i (+ i 2)))
		   
		   ((:jmp.l :brf.l :brt.l)
		    (princ "@" (hex5 (ref-uint32-LE code i)))
		    (set! i (+ i 4)))
		   
		   (else #f))))))))

#t
