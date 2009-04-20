; femtoLisp standard library
; by Jeff Bezanson
; Public Domain

(set 'list (lambda args args))

(set 'setq (macro (name val)
                  (list set (list quote name) val)))

(setq sp '| |)
(setq nl '|
|)

; convert a sequence of body statements to a single expression.
; this allows define, defun, defmacro, let, etc. to contain multiple
; body expressions as in Common Lisp.
(setq f-body (lambda (e)
               (cond ((atom e)        e)
                     ((eq (cdr e) ()) (car e))
                     (t               (cons progn e)))))

(setq defmacro
      (macro (name args . body)
             (list 'setq name (list 'macro args (f-body body)))))

; support both CL defun and Scheme-style define
(defmacro defun (name args . body)
  (list 'setq name (list 'lambda args (f-body body))))

(defmacro define (name . body)
  (if (symbolp name)
      (list 'setq name (car body))
    (cons 'defun (cons (car name) (cons (cdr name) body)))))

(defun identity (x) x)
(setq null not)
(defun consp (x) (not (atom x)))

(defun map (f lst)
  (if (atom lst) lst
    (cons (f (car lst)) (map f (cdr lst)))))

(defmacro let (binds . body)
  (cons (list 'lambda (map car binds) (f-body body))
        (map cadr binds)))

(defun nconc lsts
  (cond ((null lsts) ())
        ((null (cdr lsts)) (car lsts))
        (t ((lambda (l d) (if (null l) d
                            (prog1 l
                              (while (consp (cdr l)) (set 'l (cdr l)))
                              (rplacd l d))))
            (car lsts) (apply nconc (cdr lsts))))))

(defun append lsts
  (cond ((null lsts) ())
        ((null (cdr lsts)) (car lsts))
        (t ((label append2 (lambda (l d)
                             (if (null l) d
                               (cons (car l)
                                     (append2 (cdr l) d)))))
            (car lsts) (apply append (cdr lsts))))))

(defun member (item lst)
  (cond ((atom lst) ())
        ((eq (car lst) item) lst)
        (t (member item (cdr lst)))))

(defun macrop (e) (and (consp e) (eq (car e) 'macro) e))
(defun macrocallp (e) (and (symbolp (car e))
                           (boundp (car e))
                           (macrop (eval (car e)))))
(defun macroapply (m args) (apply (cons 'lambda (cdr m)) args))

(defun macroexpand-1 (e)
  (if (atom e) e
    (let ((f (macrocallp e)))
      (if f (macroapply f (cdr e))
        e))))

; convert to proper list, i.e. remove "dots", and append
(defun append.2 (l tail)
  (cond ((null l)  tail)
        ((atom l)  (cons l tail))
        (t         (cons (car l) (append.2 (cdr l) tail)))))

(defun macroexpand (e)
  ((label mexpand
          (lambda (e env f)
            (progn
              (while (and (consp e)
                          (not (member (car e) env))
                          (set 'f (macrocallp e)))
                (set 'e (macroapply f (cdr e))))
              (if (and (consp e)
                       (not (or (eq (car e) 'quote)
                                (eq (car e)  quote))))
                  (let ((newenv
                         (if (and (or (eq (car e) 'lambda) (eq (car e) 'macro))
                                  (consp (cdr e)))
                             (append.2 (cadr e) env)
                           env)))
                    (map (lambda (x) (mexpand x newenv nil)) e))
                e))))
   e nil nil))

; uncomment this to macroexpand functions at definition time.
; makes typical code ~25% faster, but only works for defun expressions
; at the top level.
;(defmacro defun (name args . body)
;  (list 'setq name (list 'lambda args (macroexpand (f-body body)))))

; same thing for macros. enabled by default because macros are usually
; defined at the top level.
(defmacro defmacro (name args . body)
  (list 'setq name (list 'macro args (macroexpand (f-body body)))))

(setq =   eq)
(setq eql eq)
(define (/= a b) (not (eq a b)))
(define != /=)
(define (>  a b) (< b a))
(define (<= a b) (not (< b a)))
(define (>= a b) (not (< a b)))
(define (mod x y) (- x (* (/ x y) y)))
(define (abs x)   (if (< x 0) (- x) x))
(define (truncate x) x)
(setq K prog1)  ; K combinator ;)
(define (funcall f . args) (apply f args))
(define (symbol-function sym) (eval sym))
(define (symbol-value    sym) (eval sym))

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

(define (equal a b)
  (if (and (consp a) (consp b))
      (and (equal (car a) (car b))
           (equal (cdr a) (cdr b)))
    (eq a b)))

; compare imposes an ordering on all values. yields -1 for a<b,
; 0 for a==b, and 1 for a>b. lists are compared up to the first
; point of difference.
(defun compare (a b)
  (cond ((eq a b) 0)
        ((or (atom a) (atom b)) (if (< a b) -1 1))
        (t (let ((c (compare (car a) (car b))))
             (if (not (eq c 0))
                 c
               (compare (cdr a) (cdr b)))))))

(defun every (pred lst)
  (or (atom lst)
      (and (pred (car lst))
           (every pred (cdr lst)))))

(defun any (pred lst)
  (and (consp lst)
       (or (pred (car lst))
           (any pred (cdr lst)))))

(defun listp (a) (or (eq a ()) (consp a)))

(defun length (l)
  (if (null l) 0
    (+ 1 (length (cdr l)))))

(defun nthcdr (n lst)
  (if (<= n 0) lst
    (nthcdr (- n 1) (cdr lst))))

(defun list-ref (lst n)
  (car (nthcdr n lst)))

(defun list* l
  (if (atom (cdr l))
      (car l)
    (cons (car l) (apply list* (cdr l)))))

(defun nlist* l
  (if (atom (cdr l))
      (car l)
    (rplacd l (apply nlist* (cdr l)))))

(defun lastcdr (l)
  (if (atom l) l
    (lastcdr (cdr l))))

(defun last (l)
  (cond ((atom l)        l)
        ((atom (cdr l))  l)
        (t               (last (cdr l)))))

(defun map! (f lst)
  (prog1 lst
    (while (consp lst)
      (rplaca lst (f (car lst)))
      (set 'lst (cdr lst)))))

(defun mapcar (f . lsts)
  ((label mapcar-
          (lambda (lsts)
            (cond ((null lsts) (f))
                  ((atom (car lsts)) (car lsts))
                  (t (cons (apply f (map car lsts))
                           (mapcar- (map cdr lsts)))))))
   lsts))

(defun transpose (M) (apply mapcar (cons list M)))

(defun filter (pred lst)
  (cond ((null lst) ())
        ((not (pred (car lst))) (filter pred (cdr lst)))
        (t (cons (car lst) (filter pred (cdr lst))))))

(define (foldr f zero lst)
  (if (null lst) zero
    (f (car lst) (foldr f zero (cdr lst)))))

(define (foldl f zero lst)
  (if (null lst) zero
    (foldl f (f (car lst) zero) (cdr lst))))

(define (reverse lst) (foldl cons nil lst))

(define (reduce0 f zero lst)
  (if (null lst) zero
    (reduce0 f (f zero (car lst)) (cdr lst))))

(defun reduce (f lst)
  (reduce0 f (car lst) (cdr lst)))

(define (copy-list l) (map identity l))
(define (copy-tree l)
  (if (atom l) l
    (cons (copy-tree (car l))
          (copy-tree (cdr l)))))

(define (assoc item lst)
  (cond ((atom lst) ())
        ((eq (caar lst) item) (car lst))
        (t (assoc item (cdr lst)))))

(define (nreverse l)
  (let ((prev nil))
    (while (consp l)
      (set 'l (prog1 (cdr l)
                (rplacd l (prog1 prev
                            (set 'prev l))))))
    prev))

(defmacro let* (binds . body)
  (cons (list 'lambda (map car binds)
              (cons progn
                    (nconc (map (lambda (b) (cons 'setq b)) binds)
                           body)))
        (map (lambda (x) nil) binds)))

(defmacro labels (binds . body)
  (cons (list 'lambda (map car binds)
              (cons progn
                    (nconc (map (lambda (b)
                                  (list 'setq (car b) (cons 'lambda (cdr b))))
                                binds)
                           body)))
        (map (lambda (x) nil) binds)))

(defmacro when   (c . body) (list if c (f-body body) nil))
(defmacro unless (c . body) (list if c nil (f-body body)))

(defmacro dotimes (var . body)
  (let ((v (car var))
        (cnt (cadr var)))
    (list 'let (list (list v 0))
          (list while (list < v cnt)
                (list prog1 (f-body body) (list 'setq v (list + v 1)))))))

(defun map-int (f n)
  (let ((acc nil))
    (dotimes (i n)
      (setq acc (cons (f i) acc)))
    (nreverse acc)))

; property lists
(setq *plists* nil)

(defun symbol-plist (sym)
  (cdr (or (assoc sym *plists*) '(()))))

(defun set-symbol-plist (sym lst)
  (let ((p (assoc sym *plists*)))
    (if (null p)  ; sym has no plist yet
        (setq *plists* (cons (cons sym lst) *plists*))
      (rplacd p lst))))

(defun get (sym prop)
  (let ((pl (symbol-plist sym)))
    (if pl
        (let ((pr (member prop pl)))
          (if pr (cadr pr) nil))
      nil)))

(defun put (sym prop val)
  (let ((p (assoc sym *plists*)))
    (if (null p)  ; sym has no plist yet
        (setq *plists* (cons (list sym prop val) *plists*))
      (let ((pr (member prop p)))
        (if (null pr)  ; sym doesn't have this property yet
            (rplacd p (cons prop (cons val (cdr p))))
          (rplaca (cdr pr) val)))))
  val)

; setf
; expands (setf (place x ...) v) to (mutator (f x ...) v)
; (mutator (identity x ...) v) is interpreted as (mutator x ... v)
(setq *setf-place-list*
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
        (get     put      identity)
        (aref    aset     identity)
        (symbol-function   set                identity)
        (symbol-value      set                identity)
        (symbol-plist      set-symbol-plist   identity)))

(defun setf-place-mutator (place val)
  (if (symbolp place)
      (list 'setq place val)
    (let ((mutator (assoc (car place) *setf-place-list*)))
      (if (null mutator)
          (error '|setf: error: unknown place | (car place))
        (if (eq (caddr mutator) 'identity)
            (cons (cadr mutator) (append (cdr place) (list val)))
          (list (cadr mutator)
                (cons (caddr mutator) (cdr place))
                val))))))

(defmacro setf args
  (f-body
   ((label setf-
           (lambda (args)
             (if (null args)
                 nil
               (cons (setf-place-mutator (car args) (cadr args))
                     (setf- (cddr args))))))
    args)))

(defun revappend (l1 l2) (nconc (reverse l1) l2))
(defun nreconc   (l1 l2) (nconc (nreverse l1) l2))

(defun builtinp (x)
  (and (atom x)
       (not (symbolp x))
       (not (numberp x))))

(defun self-evaluating-p (x)
  (or (eq x nil)
      (eq x t)
      (and (atom x)
           (not (symbolp x)))))

; backquote
(defmacro backquote (x) (bq-process x))

(defun splice-form-p (x)
  (or (and (consp x) (or (eq (car x) '*comma-at*)
                         (eq (car x) '*comma-dot*)))
      (eq x '*comma*)))

(defun bq-process (x)
  (cond ((self-evaluating-p x)        x)
        ((atom x)                     (list quote x))
        ((eq (car x) 'backquote)      (bq-process (bq-process (cadr x))))
        ((eq (car x) '*comma*)        (cadr x))
        ((not (any splice-form-p x))
         (let ((lc    (lastcdr x))
               (forms (map bq-bracket1 x)))
           (if (null lc)
               (cons 'list forms)
             (nconc (cons 'nlist* forms) (list (bq-process lc))))))
        (t (let ((p x) (q '()))
             (while (and (consp p)
                         (not (eq (car p) '*comma*)))
               (setq q (cons (bq-bracket (car p)) q))
               (setq p (cdr p)))
             (cons 'nconc
                   (cond ((consp p) (nreconc q (list (cadr p))))
                         ((null p)  (nreverse q))
                         (t         (nreconc q (list (bq-process p))))))))))

(defun bq-bracket (x)
  (cond ((atom x)                   (list cons (bq-process x) nil))
        ((eq (car x) '*comma*)      (list cons (cadr x)       nil))
        ((eq (car x) '*comma-at*)   (list 'copy-list (cadr x)))
        ((eq (car x) '*comma-dot*)  (cadr x))
        (t                          (list cons (bq-process x) nil))))

; bracket without splicing
(defun bq-bracket1 (x)
  (if (and (consp x) (eq (car x) '*comma*))
      (cadr x)
    (bq-process x)))
