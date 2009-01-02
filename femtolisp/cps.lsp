(define (cond->if form)
  (cond-clauses->if (cdr form)))
(define (cond-clauses->if lst)
  (if (atom lst)
      lst
    (let ((clause (car lst)))
      `(if ,(car clause)
           ,(f-body (cdr clause))
         ,(cond-clauses->if (cdr lst))))))

(define (progn->cps forms k)
  (cond ((atom forms)       `(,k ,forms))
        ((null (cdr forms)) (cps- (car forms) k))
        (T (let ((_ (gensym)))   ; var to bind ignored value
             (cps- (car forms) `(lambda (,_)
                                  ,(progn->cps (cdr forms) k)))))))

(define (rest->cps xformer form k argsyms)
  (let ((el (car form)))
    (if (or (atom el) (constantp el))
        (xformer (cdr form) k (cons el argsyms))
      (let ((g (gensym)))
        (cps- el `(lambda (,g)
                    ,(xformer (cdr form) k (cons g argsyms))))))))

; (f x) => (cps- f `(lambda (F) ,(cps- x `(lambda (X) (F ,k X)))))
(define (app->cps form k argsyms)
  (cond ((atom form)
         (let ((r (reverse argsyms)))
           `(,(car r) ,k ,@(cdr r))))
        (T (rest->cps app->cps form k argsyms))))

; (+ x) => (cps- x `(lambda (X) (,k (+ X))))
(define (builtincall->cps form k)
  (prim->cps (cdr form) k (list (car form))))
(define (prim->cps form k argsyms)
  (cond ((atom form) `(,k ,(reverse argsyms)))
        (T           (rest->cps prim->cps form k argsyms))))

(define *top-k* (gensym))
(set *top-k* identity)

(define (cps form)
  (η-reduce
   (β-reduce
    (macroexpand
     (cps- (macroexpand form) *top-k*)))))
(define (cps- form k)
  (let ((g (gensym)))
    (cond ((or (atom form) (constantp form))
           `(,k ,form))

          ((eq (car form) 'lambda)
           `(,k (lambda ,(cons g (cadr form)) ,(cps- (caddr form) g))))

          ((eq (car form) 'progn)
           (progn->cps (cdr form) k))

          ((eq (car form) 'cond)
           (cps- (cond->if form) k))

          ((eq (car form) 'if)
           (let ((test (cadr form))
                 (then (caddr form))
                 (else (cadddr form)))
             (if (atom k)
                 (cps- test `(lambda (,g)
                               (if ,g
                                   ,(cps- then k)
                                 ,(cps- else k))))
               `(let ((,g ,k))
                  ,(cps- form g)))))

          ((eq (car form) 'and)
           (cond ((atom (cdr  form)) `(,k T))
                 ((atom (cddr form)) (cps- (cadr form) k))
                 (T
                  (if (atom k)
                      (cps- (cadr form)
                            `(lambda (,g)
                               (if ,g ,(cps- `(and ,@(cddr form)) k)
                                 (,k ,g))))
                    `(let ((,g ,k))
                       ,(cps- form g))))))

          ((eq (car form) 'or)
           (cond ((atom (cdr  form)) `(,k ()))
                 ((atom (cddr form)) (cps- (cadr form) k))
                 (T
                  (if (atom k)
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
             (cps- (macroexpand
                    `(let ((,lastval nil))
                       ((label ,g (lambda ()
                                    (if ,test
                                        (progn (setq ,lastval ,body)
                                               (,g))
                                      ,lastval))))))
                   k)))

          ((eq (car form) 'setq)
           (let ((var (cadr form))
                 (E   (caddr form)))
             (cps- E `(lambda (,g) (,k (setq ,var ,g))))))

          ((eq (car form) 'reset)
           `(,k ,(cps- (cadr form) *top-k*)))

          ((eq (car form) 'shift)
           (let ((v (cadr form))
                 (E (caddr form))
                 (val (gensym)))
             `(let ((,v (lambda (,g ,val) (,g (,k ,val)))))
                ,(cps- E *top-k*))))

          ((and (constantp (car form))
                (builtinp (eval (car form))))
           (builtincall->cps form k))

          ; ((lambda (...) body) ...)
          ((and (consp (car form))
                (eq (caar form) 'lambda))
           (let ((largs (cadr (car form)))
                 (lbody (caddr (car form))))
             (if (null largs)
                 (cps- lbody k)  ; ((lambda () x))
               (cps- (cadr form) `(lambda (,(car largs))
                                    ,(cps- `((lambda ,(cdr largs) ,lbody)
                                             ,@(cddr form))
                                           k))))))

          (T
           (app->cps form k ())))))

; (lambda (args...) (f args...)) => f
; but only for constant, builtin f
(define (η-reduce form)
  (cond ((or (atom form) (constantp form)) form)
        ((and (eq (car form) 'lambda)
              (let ((body (caddr form))
                    (args (cadr form))
                    (func (car (caddr form))))
                (and (consp body)
                     (equal (cdr body) args)
                     (constantp func))))
         (η-reduce (car (caddr form))))
        (T (map η-reduce form))))

(define (contains x form)
  (or (eq form x)
      (any (lambda (p) (contains x p)) form)))

(define (β-reduce form)
  (if (or (atom form) (constantp form))
      form
    (β-reduce- (map β-reduce form))))

(define (β-reduce- form)
        ; ((lambda (f) (f arg)) X) => (X arg)
  (cond ((and (= (length form) 2)
              (consp (car form))
              (eq (caar form) 'lambda)
              (let ((args (cadr (car form)))
                    (body (caddr (car form))))
                (and (consp body)
                     (= (length body) 2)
                     (= (length args) 1)
                     (eq (car body) (car args))
                     (not (eq (cadr body) (car args)))
                     (symbolp (cadr body)))))
         `(,(cadr form)
           ,(cadr (caddr (car form)))))

        ; (identity x) => x
        ((eq (car form) *top-k*)
         (cadr form))

        ; uncurry:
        ; ((lambda (p1) ((lambda (args...) body) exprs...)) s) =>
        ; ((lambda (p1 args...) body) s exprs...)
        ; where exprs... doesn't contain p1
        ((and (= (length form) 2)
              (consp (car form))
              (eq (caar form) 'lambda)
              (or (atom (cadr form)) (constantp (cadr form)))
              (let ((args (cadr (car form)))
                    (s (cadr form))
                    (body (caddr (car form))))
                (and (= (length args) 1)
                     (consp body)
                     (consp (car body))
                     (eq (caar body) 'lambda)
                     (let ((innerargs (cadr (car body)))
                           (innerbody (caddr (car body)))
                           (params (cdr body)))
                       (and (not (contains (car args) params))
                            `((lambda ,(cons (car args) innerargs)
                                ,innerbody)
                              ,s
                              ,@params)))))))

        (T form)))

(defmacro with-delimited-continuations code (cps (f-body code)))

(defmacro defgenerator (name args . body)
  (let ((ko  (gensym))
        (cur (gensym)))
    `(defun ,name ,args
       (let ((,ko  ())
             (,cur ()))
         (lambda ()
           (with-delimited-continuations
            (if ,ko (,ko ,cur)
              (reset
               (let ((yield
                      (lambda (v)
                        (shift yk
                               (progn (setq ,ko  yk)
                                      (setq ,cur v))))))
                 ,(f-body body))))))))))

; a test case
(defgenerator range-iterator (lo hi)
  ((label loop
          (lambda (i)
            (if (< hi i)
                'done
              (progn (yield i)
                     (loop (+ 1 i))))))
   lo))

; example from Chung-chieh Shan's paper
(assert (equal
         (with-delimited-continuations
          (cons 'a (reset (cons 'b (shift f (cons 1 (f (f (cons 'c ())))))))))
         '(a 1 b b c)))

T

#|
todo:
- tag lambdas that accept continuation arguments, compile computed
  calls to calls to funcall/cc that does the right thing for both
  cc-lambdas and normal lambdas

- handle dotted arglists in lambda

- use fewer gensyms

 here's an alternate way to transform a while loop:

 (let ((x 0))
   (while (< x 10)
     (progn (#.print x) (setq x (+ 1 x)))))
 =>
  (let ((x 0))
    (reset
     (let ((l nil))
       (let ((k (shift k (k k))))
         (if (< x 10)
             (progn (setq l (progn (#.print x)
                                   (setq x (+ 1 x))))
                    (k k))
           l)))))
|#
