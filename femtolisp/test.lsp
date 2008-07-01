; make label self-evaluating, but evaluating the lambda in the process
;(defmacro labl (name f)
;  (list list ''labl (list 'quote name) f))

(defmacro labl (name f)
  `(let (,name) (set ',name ,f)))

;(define (reverse lst)
;  ((label rev-help (lambda (lst result)
;                     (if (null lst) result
;                       (rev-help (cdr lst) (cons (car lst) result)))))
;   lst nil))

(define (append- . lsts)
  ((label append-h
          (lambda (lsts)
            (cond ((null lsts) ())
                  ((null (cdr lsts)) (car lsts))
                  (T ((label append2 (lambda (l d)
                                       (if (null l) d
                                         (cons (car l)
                                               (append2 (cdr l) d)))))
                      (car lsts) (append-h (cdr lsts)))))))
   lsts))

;(princ 'Hello '| | 'world! "\n")
;(filter (lambda (x) (not (< x 0))) '(1 -1 -2 5 10 -8 0))
(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
;(princ (time (fib 34)) "\n")
;(dotimes (i 20000) (map-int (lambda (x) (list 'quote x)) 8))
;(dotimes (i 40000) (append '(a b) '(1 2 3 4) nil '(c) nil '(5 6)))
;(dotimes (i 80000) (list 1 2 3 4 5))
;(setq a (map-int identity 10000))
;(dotimes (i 200) (rfoldl cons nil a))

; iterative filter
(defun ifilter (pred lst)
  ((label f (lambda (accum lst)
              (cond ((null lst) (nreverse accum))
                    ((not (pred (car lst))) (f accum (cdr lst)))
                    (T (f (cons (car lst) accum) (cdr lst))))))
   nil lst))

(defun sort (l)
  (if (or (null l) (null (cdr l))) l
    (let ((piv (car l)))
      (nconc (sort (filter (lambda (x) (<= x piv)) (cdr l)))
             (list piv)
             (sort (filter (lambda (x) (>  x piv)) (cdr l)))))))

;(setq r (map-int (lambda (x) (mod (+ (* x 9421) 12345) 1024)) 1000))
;(sort r)

(defmacro dotimes (var . body)
  (let ((v   (car var))
        (cnt (cadr var)))
    `(let ((,v 0))
       (while (< ,v ,cnt)
         (prog1
             ,(f-body body)
           (setq ,v (+ ,v 1)))))))

(defmacro labl (name fn)
  (list (list lambda (cons name nil) (list 'setq name fn)) nil))

;(dotimes (n 5000) (macroexpand '(dotimes (i 100) body1 body2)))

(define (square x) (* x x))
(define (evenp  x) (= x (* (/ x 2) 2)))
(define (expt b p)
  (cond ((= p 0) 1)
        ((= b 0) 0)
        ((evenp p) (square (expt b (/ p 2))))
        (T (* b (expt b (- p 1))))))

(define (gcd a b)
  (cond ((= a 0) b)
        ((= b 0) a)
        ((< a b)  (gcd a (- b a)))
        (T        (gcd b (- a b)))))

; like eval-when-compile
(defmacro literal (expr)
  (let ((v (eval expr)))
    (if (self-evaluating-p v) v (list quote v))))

(defun cardepth (l)
  (if (atom l) 0
    (+ 1 (cardepth (car l)))))

(defun nestlist (f zero n)
  (if (<= n 0) ()
    (cons zero (nestlist f (f zero) (- n 1)))))

(defun mapl (f . lsts)
  ((label mapl-
          (lambda (lsts)
            (if (null (car lsts)) ()
              (progn (apply f lsts) (mapl- (map cdr lsts))))))
   lsts))

; test to see if a symbol begins with :
(defun keywordp (s)
  (and (>= s '|:|) (<= s '|:~|)))

; swap the cars and cdrs of every cons in a structure
(defun swapad (c)
  (if (atom c) c
    (rplacd c (K (swapad (car c))
                 (rplaca c (swapad (cdr c)))))))

(defun without (x l)
  (filter (lambda (e) (not (eq e x))) l))

(defun conscount (c)
  (if (consp c) (+ 1
                   (conscount (car c))
                   (conscount (cdr c)))
    0))

;  _ Welcome to
; (_ _ _ |_ _ |  . _ _ 2
; | (-||||_(_)|__|_)|_)
; ==================|==

;[` _ ,_ |-  | . _  2
;| (/_||||_()|_|_\|)
;                 | 

(defmacro while- (test . forms)
  `((label -loop- (lambda ()
                    (if ,test
                        (progn ,@forms
                               (-loop-))
                      nil)))))

; this would be a cool use of thunking to handle 'finally' clauses, but
; this code doesn't work in the case where the user manually re-raises
; inside a catch block. one way to handle it would be to replace all
; their uses of 'raise' with '*_try_finally_raise_*' which calls the thunk.
; (try expr
;      (catch (TypeError e) . exprs)
;      (catch (IOError e) . exprs)
;      (finally . exprs))
(defmacro try (expr . forms)
  (let ((final (f-body (cdr (or (assoc 'finally forms) '(())))))
        (body (foldr
               ; create a function to check for and handle one exception
               ; type, and pass off control to the next when no match
               (lambda (catc next)
                 (let ((var    (cadr (cadr catc)))
                       (extype (caadr catc))
                       (todo   (f-body (cddr  catc))))
                   `(lambda (,var)
                      (if (or (eq ,var ',extype)
                              (and (consp ,var)
                                   (eq (car ,var) ',extype)))
                          ,todo
                        (,next ,var)))))

               ; default function; no matches so re-raise
               '(lambda (e) (progn (*_try_finally_thunk_*) (raise e)))

               ; make list of catch forms
               (filter (lambda (f) (eq (car f) 'catch)) forms))))
    `(let ((*_try_finally_thunk_* (lambda () ,final)))
       (prog1 (attempt ,expr ,body)
         (*_try_finally_thunk_*)))))

(defun map (f lst)
  (if (atom lst) lst
    (cons (funcall f (car lst)) (map f (cdr lst)))))

(define Y
  (lambda (f)
    ((lambda (h)
       (f (lambda (x) ((h h) x))))
     (lambda (h)
       (f (lambda (x) ((h h) x)))))))

(defmacro debug ()
  (let ((g (gensym)))
    `(progn (princ "Debug REPL:\n")
            (let ((,g (read)))
              (while (not (eq ,g 'quit))
                (prog1
                    (print (trycatch (apply '(macro x x) ,g)
                                     identity))
                  (setq ,g (read))))))))

(defun tt () (time (dotimes (i 500000) (* 0x1fffffff 1) )))
(tt)
(tt)
(tt)
