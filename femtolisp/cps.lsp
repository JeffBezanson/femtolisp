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
  (let ((g (gensym)))
    (cps- (car form) `(lambda (,g)
                        ,(xformer (cdr form) k (cons g argsyms))))))

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

(define (cps form)
  (η-reduce
   (β-reduce
    (macroexpand
     (cps- (macroexpand form) 'identity)))))
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

          ((eq (car form) 'setq)
           (let ((var (cadr form))
                 (E   (caddr form)))
             (cps- E `(lambda (,g) (,k (setq ,var ,g))))))

          ((eq (car form) 'reset)
           `(,k ,(cps- (cadr form) 'identity)))

          ((eq (car form) 'shift)
           (let ((v (cadr form))
                 (E (caddr form)))
             `(let ((,v (lambda (ignored-k val) (,k val))))
                ,(cps- E 'identity))))

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
(define (η-reduce form)
  (cond ((or (atom form) (constantp form)) form)
        ((and (eq (car form) 'lambda)
              (let ((body (caddr form))
                    (args (cadr form)))
                (and (consp body)
                     (equal (cdr body) args))))
         (η-reduce (car (caddr form))))
        (T (map η-reduce form))))

; ((lambda (f) (f arg)) X) => (X arg)
(define (β-reduce form)
  (cond ((or (atom form) (constantp form)) form)
        ((and (= (length form) 2)
              (consp (car form))
              (eq (caar form) 'lambda)
              (let ((args (cadr (car form)))
                    (body (caddr (car form))))
                (and (= (length body) 2)
                     (= (length args) 1)
                     (eq (car body) (car args))
                     (not (eq (cadr body) (car args)))
                     (symbolp (cadr body)))))
         `(,(β-reduce (cadr form))
           ,(cadr (caddr (car form)))))
        (T (map β-reduce form))))

(defmacro with-delimited-continuations (exp) (cps exp))

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

T

#|
todo:
- tag lambdas that accept continuation arguments, compile computed
  calls to calls to funcall/cc that does the right thing for both
  cc-lambdas and normal lambdas

- handle while, and, or
|#
