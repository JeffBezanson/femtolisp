; -*- scheme -*-
(define (begin->cps forms k)
  (cond ((atom? forms)       `(,k ,forms))
        ((null? (cdr forms))  (cps- (car forms) k))
        (#t (let ((_ (gensym)))   ; var to bind ignored value
	      (cps- (car forms) `(lambda (,_)
				   ,(begin->cps (cdr forms) k)))))))

(define-macro (lambda/cc args body)
  `(cons 'lambda/cc (lambda ,args ,body)))

; a utility used at run time to dispatch a call with or without
; the continuation argument, depending on the function
(define (funcall/cc f k . args)
  (if (and (pair? f) (eq (car f) 'lambda/cc))
      (apply (cdr f) (cons k args))
      (k (apply f args))))
(define *funcall/cc-names*
  (list->vector
   (map (lambda (i) (symbol (string 'funcall/cc- i)))
        (iota 6))))
(define-macro (def-funcall/cc-n args)
  (let ((name (aref *funcall/cc-names* (length args))))
    `(define (,name f k ,@args)
       (if (and (pair? f) (eq (car f) 'lambda/cc))
           ((cdr f) k ,@args)
	   (k (f ,@args))))))
(def-funcall/cc-n ())
(def-funcall/cc-n (a0))
(def-funcall/cc-n (a0 a1))
(def-funcall/cc-n (a0 a1 a2))
(def-funcall/cc-n (a0 a1 a2 a3))
(def-funcall/cc-n (a0 a1 a2 a3 a4))

(define (rest->cps xformer form k argsyms)
  (let ((el (car form)))
    (if (or (atom? el) (constant? el))
        (xformer (cdr form) k (cons el argsyms))
      (let ((g (gensym)))
        (cps- el `(lambda (,g)
                    ,(xformer (cdr form) k (cons g argsyms))))))))

(define (make-funcall/cc head ke args)
  (let ((n (length args)))
    (if (< n 6)
        `(,(aref *funcall/cc-names* n) ,head ,ke ,@args)
      `(funcall/cc ,head ,ke ,@args))))

; (f x) => (cps- f `(lambda (F) ,(cps- x `(lambda (X) (F ,k X)))))
(define (app->cps form k argsyms)
  (cond ((atom? form)
         (let ((r (reverse argsyms)))
           (make-funcall/cc (car r) k (cdr r))))
        (#t (rest->cps app->cps form k argsyms))))

; (+ x) => (cps- x `(lambda (X) (,k (+ X))))
(define (builtincall->cps form k)
  (prim->cps (cdr form) k (list (car form))))
(define (prim->cps form k argsyms)
  (cond ((atom? form) `(,k ,(reverse argsyms)))
        (#t           (rest->cps prim->cps form k argsyms))))

(define *top-k* (gensym))
(set-top-level-value! *top-k* identity)

(define (cps form)
  (η-reduce
   (β-reduce
    (expand
     (cps- (expand form) *top-k*)))))
(define (cps- form k)
  (let ((g (gensym)))
    (cond ((or (atom? form) (constant? form))
           `(,k ,form))

          ((eq (car form) 'lambda)
           `(,k (lambda/cc ,(cons g (cadr form)) ,(cps- (caddr form) g))))

          ((eq (car form) 'begin)
           (begin->cps (cdr form) k))

          ((eq (car form) 'if)
           (let ((test (cadr form))
                 (then (caddr form))
                 (else (cadddr form)))
             (if (atom? k)
                 (cps- test `(lambda (,g)
                               (if ,g
                                   ,(cps- then k)
                                 ,(cps- else k))))
               `(let ((,g ,k))
                  ,(cps- form g)))))

          ((eq (car form) 'and)
           (cond ((atom? (cdr  form)) `(,k #t))
                 ((atom? (cddr form)) (cps- (cadr form) k))
                 (#t
                  (if (atom? k)
                      (cps- (cadr form)
                            `(lambda (,g)
                               (if ,g ,(cps- `(and ,@(cddr form)) k)
                                 (,k ,g))))
                    `(let ((,g ,k))
                       ,(cps- form g))))))

          ((eq (car form) 'or)
           (cond ((atom? (cdr  form)) `(,k #f))
                 ((atom? (cddr form)) (cps- (cadr form) k))
                 (#t
                  (if (atom? k)
                      (cps- (cadr form)
                            `(lambda (,g)
                               (if ,g (,k ,g)
                                 ,(cps- `(or ,@(cddr form)) k))))
                    `(let ((,g ,k))
                       ,(cps- form g))))))

          ((eq (car form) 'while)
           (let ((test (cadr form))
                 (body (caddr form))
                 (lastval (gensym)))
             (cps- (expand
                    `(let ((,lastval #f))
                       ((label ,g (lambda ()
                                    (if ,test
                                        (begin (set! ,lastval ,body)
                                               (,g))
                                      ,lastval))))))
                   k)))

          ((eq (car form) 'set!)
           (let ((var (cadr form))
                 (E   (caddr form)))
             (cps- E `(lambda (,g) (,k (set! ,var ,g))))))

          ((eq (car form) 'reset)
           `(,k ,(cps- (cadr form) *top-k*)))

          ((eq (car form) 'shift)
           (let ((v (cadr form))
                 (E (caddr form))
                 (val (gensym)))
             `(let ((,v (lambda/cc (,g ,val) (,g (,k ,val)))))
                ,(cps- E *top-k*))))

          ((eq (car form) 'without-delimited-continuations)
           `(,k ,(cadr form)))

          ((and (constant? (car form))
                (builtin? (eval (car form))))
           (builtincall->cps form k))

          ; ((lambda (...) body) ...)
          ((and (pair? (car form))
                (eq (caar form) 'lambda))
           (let ((largs (cadr (car form)))
                 (lbody (caddr (car form))))
             (cond ((null? largs)   ; ((lambda () body))
                    (cps- lbody k))
                   ((symbol? largs) ; ((lambda x body) args...)
                    (cps- `((lambda (,largs) ,lbody) (list ,@(cdr form))) k))
                   (#t
                    (cps- (cadr form) `(lambda (,(car largs))
                                         ,(cps- `((lambda ,(cdr largs) ,lbody)
                                                  ,@(cddr form))
                                                k)))))))

          (#t
           (app->cps form k ())))))

; (lambda (args...) (f args...)) => f
; but only for constant, builtin f
(define (η-reduce form)
  (cond ((or (atom? form) (constant? form)) form)
        ((and (eq (car form) 'lambda)
              (let ((body (caddr form))
                    (args (cadr form)))
                (and (pair? body)
                     (equal? (cdr body) args)
                     (constant? (car (caddr form))))))
         (car (caddr form)))
        (#t (map η-reduce form))))

(define (contains x form)
  (or (eq form x)
      (any (lambda (p) (contains x p)) form)))

(define (β-reduce form)
  (if (or (atom? form) (constant? form))
      form
      (β-reduce- (map β-reduce form))))

(define (β-reduce- form)
        ; ((lambda (f) (f arg)) X) => (X arg)
  (cond ((and (length= form 2)
              (pair? (car form))
              (eq (caar form) 'lambda)
              (let ((args (cadr (car form)))
                    (body (caddr (car form))))
                (and (pair? body) (pair? args)
                     (length= body 2)
                     (length= args 1)
                     (eq (car body) (car args))
                     (not (eq (cadr body) (car args)))
                     (symbol? (cadr body)))))
         `(,(cadr form)
           ,(cadr (caddr (car form)))))

        ; (identity x) => x
        ((eq (car form) *top-k*)
         (cadr form))

        ; uncurry:
        ; ((lambda (p1) ((lambda (args...) body) exprs...)) s) =>
        ; ((lambda (p1 args...) body) s exprs...)
        ; where exprs... doesn't contain p1
        ((and (length= form 2)
              (pair? (car form))
              (eq (caar form) 'lambda)
              (or (atom? (cadr form)) (constant? (cadr form)))
              (let ((args (cadr (car form)))
                    (s (cadr form))
                    (body (caddr (car form))))
                (and (pair? args) (length= args 1)
                     (pair? body)
                     (pair? (car body))
                     (eq (caar body) 'lambda)
                     (let ((innerargs (cadr (car body)))
                           (innerbody (caddr (car body)))
                           (params (cdr body)))
                       (and (not (contains (car args) params))
                            `((lambda ,(cons (car args) innerargs)
                                ,innerbody)
                              ,s
                              ,@params)))))))

        (#t form)))

(define-macro (with-delimited-continuations . code)
  (cps `((lambda () ,@code))))

(define-macro (define-generator form . body)
  (let ((ko  (gensym))
        (cur (gensym))
	(name (car form))
	(args (cdr form)))
    `(define (,name ,@args)
       (let ((,ko  #f)
             (,cur #f))
         (lambda ()
           (with-delimited-continuations
            (if ,ko (,ko ,cur)
              (reset
               (let ((yield
                      (lambda (v)
                        (shift yk
                               (begin (set! ,ko  yk)
                                      (set! ,cur v))))))
                 ,@body)))))))))

; a test case
(define-generator (range-iterator lo hi)
  ((label loop
          (lambda (i)
            (if (< hi i)
                'done
              (begin (yield i)
                     (loop (+ 1 i))))))
   lo))

; example from Chung-chieh Shan's paper
(assert (equal?
         (with-delimited-continuations
          (cons 'a (reset (cons 'b (shift f (cons 1 (f (f (cons 'c ())))))))))
         '(a 1 b b c)))

#t

#|
todo:
* tag lambdas that accept continuation arguments, compile computed
  calls to calls to funcall/cc that does the right thing for both
  cc-lambdas and normal lambdas

* handle dotted arglists in lambda

- optimize constant functions, e.g. (funcall/cc-0 #:g65 (lambda (#:g58) 'done))

- implement CPS version of apply

- use fewer gensyms

 here's an alternate way to transform a while loop:

 (let ((x 0))
   (while (< x 10)
     (begin (print x) (set! x (+ 1 x)))))
 =>
  (let ((x 0))
    (reset
     (let ((l #f))
       (let ((k (shift k (k k))))
         (if (< x 10)
             (begin (set! l (begin (print x)
                                   (set! x (+ 1 x))))
                    (k k))
           l)))))
|#
