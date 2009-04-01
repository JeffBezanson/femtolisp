; -*- scheme -*-

(define (make-enum-table keys)
  (let ((e (table)))
    (for 0 (1- (length keys))
	 (lambda (i)
	   (put! e (aref keys i) i)))))

(define Instructions
  (make-enum-table
   [:nop :dup :pop :popn :call :jmp :brf :brt :jmp.l :brf.l :brt.l :ret

    :eq? :eqv? :equal? :atom? :not :null? :boolean? :symbol?
    :number? :bound? :pair? :builtin? :vector? :fixnum?

    :cons :list :car :cdr :set-car! :set-cdr!
    :eval :eval* :apply

    :+ :- :* :/ :< :lognot :compare

    :vector :aref :aset :length :for

    :loadt :loadf :loadnil :load0 :load1 :loadv :loadv.l
    :loadg :loada :loadc
    :setg  :seta  :setc  :loadg.l :setg.l

    :closure :trycatch]))

(define 1/Instructions (table.invert Instructions))

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

(define (count- f l n)
  (if (null? l)
      n
      (count- f (cdr l) (if (f (car l))
			    (+ n 1)
			    n))))
(define (count f l) (count- f l 0))

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
			
			((:loada :seta :call :loadv :loadg :setg :popn
				 :list :+ :- :* :/ :vector)
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
  (let ((const-to-idx (aref e 1))
	(nconst       (aref e 2)))
    (let ((cvec (vector.alloc nconst)))
      (table.foreach (lambda (val idx) (aset! cvec idx val))
		     const-to-idx)
      cvec)))

(define (bytecode g)
  (cons (encode-byte-code (aref g 0))
	(const-to-idx-vec g)))

(define (bytecode:code b) (car b))
(define (bytecode:vals b) (cdr b))

(define (index-of item lst start)
  (cond ((null? lst) #f)
	((eq item (car lst)) start)
	(#t (index-of item (cdr lst) (+ start 1)))))

(define (in-env? s env)
  (and (pair? env)
       (or (index-of s (car env) 0)
	   (in-env? s (cdr env)))))

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
			(if (null? curr) lev (+ lev 1))
			#f)))))

(define (compile-sym g s env Is)
  (let ((loc (lookup-sym s env 0 #t)))
    (case (car loc)
      (arg     (emit g (aref Is 0) (cadr loc)))
      (closed  (emit g (aref Is 1) (cadr loc) (caddr loc)))
      (else    (emit g (aref Is 2) s)))))

(define (builtin->instruction b)
  (let ((sym (intern (string #\: b))))
    (and (has? Instructions sym) sym)))

(define (cond->if form)
  (cond-clauses->if (cdr form)))
(define (cond-clauses->if lst)
  (if (atom? lst)
      lst
    (let ((clause (car lst)))
      (if (eq? (car clause) 'else)
	  (cons 'begin (cdr clause))
	  `(if ,(car clause)
	       ,(cons 'begin (cdr clause))
	       ,(cond-clauses->if (cdr lst)))))))

(define (compile-if g x env)
  (let ((elsel (make-label g))
	(endl  (make-label g)))
    (compile-in g (cadr x) env)
    (emit g :brf elsel)
    (compile-in g (caddr x) env)
    (emit g :jmp endl)
    (mark-label g elsel)
    (compile-in g (if (pair? (cdddr x))
		      (cadddr x)
		      #f)
		env)
    (mark-label g endl)))

(define (compile-begin g forms env)
  (cond ((atom? forms) (compile-in g #f env))
	((atom? (cdr forms))
	 (compile-in g (car forms) env))
	(else
	 (compile-in g (car forms) env)
	 (emit g :pop)
	 (compile-begin g (cdr forms) env))))

(define (compile-prog1 g x env)
  (compile-in g (cadr x) env)
  (if (pair? (cddr x))
      (begin (compile-begin g (cddr x) env)
	     (emit g :pop))))

(define (compile-while g cond body env)
  (let ((top  (make-label g))
	(end  (make-label g)))
    (mark-label g top)
    (compile-in g cond env)
    (emit g :brf end)
    (compile-in g body env)
    (emit g :pop)
    (emit g :jmp top)
    (mark-label g end)))

(define (compile-and g forms env)
  (cond ((atom? forms)        (compile-in g #t env))
	((atom? (cdr forms))  (compile-in g (car forms) env))
	(else
	 (let ((end  (make-label g)))
	   (compile-in g (car forms) env)
	   (emit g :dup)
	   (emit g :brf end)
	   (emit g :pop)
	   (compile-and g (cdr forms) env)
	   (mark-label g end)))))

(define (compile-or g forms env)
  (cond ((atom? forms)        (compile-in g #f env))
	((atom? (cdr forms))  (compile-in g (car forms) env))
	(else
	 (let ((end  (make-label g)))
	   (compile-in g (car forms) env)
	   (emit g :dup)
	   (emit g :brt end)
	   (emit g :pop)
	   (compile-or g (cdr forms) env)
	   (mark-label g end)))))

(define MAX_ARGS 127)

(define (list-part- l n  i subl acc)
  (cond ((atom? l) (if (> i 0)
		       (cons (nreverse subl) acc)
		       acc))
	((>= i n)  (list-part- l n 0 () (cons (nreverse subl) acc)))
	(else      (list-part- (cdr l) n (+ 1 i) (cons (car l) subl) acc))))
(define (list-partition l n)
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
	      (compile-in g a env))
	    lst))

(define (compile-arglist g lst env)
  (let ((argtail (length> lst MAX_ARGS)))
    (if argtail
	(begin (just-compile-args g (list-head lst MAX_ARGS) env)
	       (let ((rest
		      (cons nconc
			    (map (lambda (l) (cons list l))
				 (list-partition argtail MAX_ARGS)))))
		 (compile-in g rest env))
	       (+ MAX_ARGS 1))
	(begin (just-compile-args g lst env)
	       (length lst)))))

(define (compile-app g x env)
  (let ((head  (car x)))
    (let ((head
	   (if (and (symbol? head)
		    (not (in-env? head env))
		    (bound? head)
		    (constant? head)
		    (builtin? (eval head)))
	       (eval head)
	       head)))
      (let ((b (and (builtin? head)
		    (builtin->instruction head))))
	(if (not b)
	    (compile-in g head env))
	(let ((nargs (compile-arglist g (cdr x) env)))
	  (if b  ;; TODO check arg count
	      (if (memq b '(:list :+ :- :* :/ :vector))
		  (emit g b nargs)
		  (emit g b))
	      (emit g :call nargs)))))))

(define (compile-in g x env)
  (cond ((symbol? x) (compile-sym g x env [:loada :loadc :loadg]))
	((atom? x)
	 (cond ((eq? x 0)  (emit g :load0))
	       ((eq? x 1)  (emit g :load1))
	       ((eq? x #t) (emit g :loadt))
	       ((eq? x #f) (emit g :loadf))
	       ((eq? x ()) (emit g :loadnil))
	       (else       (emit g :loadv x))))
	(else
	 (case (car x)
	   (quote    (emit g :loadv (cadr x)))
	   (cond     (compile-in g (cond->if x) env))
	   (if       (compile-if g x env))
	   (begin    (compile-begin g (cdr x) env))
	   (prog1    (compile-prog1 g x env))
	   (lambda   (begin (emit g :loadv (compile-f x env))
			    (emit g :closure)))
	   (and      (compile-and g (cdr x) env))
	   (or       (compile-or  g (cdr x) env))
	   (while    (compile-while g (cadr x) (caddr x) env))
	   (set!     (compile-in g (caddr x) env)
		     (compile-sym g (cadr x) env [:seta :setc :setg]))
	   (trycatch (compile-in g `(lambda () ,(cadr x)) env)
		     (compile-in g (caddr x))
		     (emit g :trycatch))
	   (else   (compile-app g x env))))))

(define (compile-f f env)
  (let ((g (make-code-emitter)))
    (compile-in g (caddr f) (cons (to-proper (cadr f)) env))
    (emit g :ret)
    `(compiled-lambda ,(cadr f) ,(bytecode g))))

(define (compile x)
  (bytecode (compile-in (make-code-emitter) x ())))

(define (ref-uint32-LE a i)
  (+ (ash (aref a (+ i 0)) 0)
     (ash (aref a (+ i 1)) 8)
     (ash (aref a (+ i 2)) 16)
     (ash (aref a (+ i 3)) 24)))

(define (ref-uint16-LE a i)
  (+ (ash (aref a (+ i 0)) 0)
     (ash (aref a (+ i 1)) 8)))

(define (hex5 n)
  (pad-l (number->string n 16) 5 #\0))

(define (disassemble- b lev)
  (if (and (pair? b)
	   (eq? (car b) 'compiled-lambda))
      (disassemble- (caddr b) lev)
      (let ((code (bytecode:code b))
	    (vals (bytecode:vals b)))
	(define (print-val v)
	  (if (and (pair? v) (eq? (car v) 'compiled-lambda))
	      (begin (princ "\n")
		     (disassemble- v (+ lev 1)))
	      (print v)))
	(let ((i 0)
	      (N (length code)))
	  (while (< i N)
		 (let ((inst (get 1/Instructions (aref code i))))
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

		     ((:loada :seta :call :popn :list :+ :- :* :/ :vector)
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

(define (disassemble b) (disassemble- b 0) (newline))

#t
