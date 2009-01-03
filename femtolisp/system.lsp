; femtoLisp standard library
; by Jeff Bezanson (C) 2008
; Distributed under the BSD License

; convert a sequence of body statements to a single expression.
; this allows define, defun, defmacro, let, etc. to contain multiple
; body expressions as in Common Lisp.
(setq f-body (lambda (e)
               (cond ((atom e)        e)
                     ((eq (cdr e) ()) (car e))
                     (T               (cons 'progn e)))))

(set-syntax 'defmacro
            (lambda (name args . body)
              (list 'set-syntax (list 'quote name)
                    (list 'lambda args (f-body body)))))

(defmacro label (name fn)
  (list (list 'lambda (list name) (list 'setq name fn)) nil))

; support both CL defun and Scheme-style define
(defmacro defun (name args . body)
  (list 'setq name (list 'lambda args (f-body body))))

(defmacro define (name . body)
  (if (symbolp name)
      (list 'setq name (car body))
    (cons 'defun (cons (car name) (cons (cdr name) body)))))

(defun set (s v) (eval (list 'setq s (list 'quote v))))

(defun identity (x) x)
(setq null not)

(defun map (f lst)
  (if (atom lst) lst
    (cons (f (car lst)) (map f (cdr lst)))))

(defmacro let (binds . body)
  (cons (list 'lambda
              (map (lambda (c) (if (consp c) (car c) c)) binds)
              (f-body body))
        (map (lambda (c) (if (consp c) (cadr c) nil)) binds)))

(defun nconc lsts
  (cond ((null lsts) ())
        ((null (cdr lsts)) (car lsts))
        ((null (car lsts)) (apply nconc (cdr lsts)))
        (T (prog1 (car lsts)
             (rplacd (last (car lsts))
                     (apply nconc (cdr lsts)))))))

(defun append lsts
  (cond ((null lsts) ())
        ((null (cdr lsts)) (car lsts))
        (T ((label append2 (lambda (l d)
                             (if (null l) d
                               (cons (car l)
                                     (append2 (cdr l) d)))))
            (car lsts) (apply append (cdr lsts))))))

(defun member (item lst)
  (cond ((atom lst) ())
        ((equal (car lst) item) lst)
        (T (member item (cdr lst)))))

(defun macrocallp (e) (and (symbolp (car e))
                           (symbol-syntax (car e))))

(defun functionp (x)
  (or (builtinp x)
      (and (consp x) (eq (car x) 'lambda))))

(defun macroexpand-1 (e)
  (if (atom e) e
    (let ((f (macrocallp e)))
      (if f (apply f (cdr e))
        e))))

; convert to proper list, i.e. remove "dots", and append
(defun append.2 (l tail)
  (cond ((null l)  tail)
        ((atom l)  (cons l tail))
        (T         (cons (car l) (append.2 (cdr l) tail)))))

(define (cadr x) (car (cdr x)))

;(setq *special-forms* '(quote cond if and or while lambda trycatch
;                        setq progn))

(defun macroexpand (e)
  ((label mexpand
          (lambda (e env f)
            (progn
              (while (and (consp e)
                          (not (member (car e) env))
                          (setq f (macrocallp e)))
                (setq e (apply f (cdr e))))
              (cond ((and (consp e)
                          (not (eq (car e) 'quote)))
                     (let ((newenv
                            (if (and (eq (car e) 'lambda)
                                     (consp (cdr e)))
                                (append.2 (cadr e) env)
                              env)))
                       (map (lambda (x) (mexpand x newenv nil)) e)))
                    ;((and (symbolp e) (constantp e)) (eval e))
                    ;((and (symbolp e)
                    ;      (not (member e *special-forms*))
                    ;      (not (member e env))) (cons '%top e))
                    (T e)))))
   e nil nil))

; uncomment this to macroexpand functions at definition time.
; makes typical code ~25% faster, but only works for defun expressions
; at the top level.
(defmacro defun (name args . body)
  (list 'setq name (macroexpand (list 'lambda args (f-body body)))))

; same thing for macros. enabled by default because macros are usually
; defined at the top level.
(defmacro defmacro (name args . body)
  (list 'set-syntax (list 'quote name)
        (macroexpand (list 'lambda args (f-body body)))))

(setq =   equal)
(setq eql equal)
(define (/= a b) (not (equal a b)))
(define != /=)
(define (>  a b) (< b a))
(define (<= a b) (not (< b a)))
(define (>= a b) (not (< a b)))
(define (1+ n) (+ n 1))
(define (1- n) (- n 1))
(define (mod x y) (- x (* (/ x y) y)))
(define (abs x)   (if (< x 0) (- x) x))
(setq K prog1)  ; K combinator ;)
(define (funcall f . args) (apply f args))
(define (symbol-value sym) (eval sym))
(define symbol-function symbol-value)
(define (terpri) (princ "\n") nil)

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

(defun every (pred lst)
  (or (atom lst)
      (and (pred (car lst))
           (every pred (cdr lst)))))

(defun any (pred lst)
  (and (consp lst)
       (or (pred (car lst))
           (any pred (cdr lst)))))

(defun listp (a) (or (eq a ()) (consp a)))

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
        (T               (last (cdr l)))))

(defun map! (f lst)
  (prog1 lst
    (while (consp lst)
      (rplaca lst (f (car lst)))
      (setq lst (cdr lst)))))

(defun mapcar (f . lsts)
  ((label mapcar-
          (lambda (lsts)
            (cond ((null lsts) (f))
                  ((atom (car lsts)) (car lsts))
                  (T (cons (apply f (map car lsts))
                           (mapcar- (map cdr lsts)))))))
   lsts))

(defun transpose (M) (apply mapcar (cons list M)))

(defun filter (pred lst) (filter- pred lst nil))
(defun filter- (pred lst accum)
  (cond ((null lst) accum)
        ((pred (car lst))
         (filter- pred (cdr lst) (cons (car lst) accum)))
        (T
         (filter- pred (cdr lst) accum))))

(defun separate (pred lst) (separate- pred lst nil nil))
(defun separate- (pred lst yes no)
  (cond ((null lst) (cons yes no))
        ((pred (car lst))
         (separate- pred (cdr lst) (cons (car lst) yes) no))
        (T
         (separate- pred (cdr lst) yes (cons (car lst) no)))))

(define (foldr f zero lst)
  (if (null lst) zero
    (f (car lst) (foldr f zero (cdr lst)))))

(define (foldl f zero lst)
  (if (null lst) zero
    (foldl f (f (car lst) zero) (cdr lst))))

(define (reverse lst) (foldl cons nil lst))

(defun reduce (f zero lst)
  (if (null lst) zero
    (reduce f (f zero (car lst)) (cdr lst))))

(define (copy-list l)
  (if (atom l) l
    (cons (car l)
          (copy-list (cdr l)))))
(define (copy-tree l)
  (if (atom l) l
    (cons (copy-tree (car l))
          (copy-tree (cdr l)))))

(define (nreverse l)
  (let ((prev nil))
    (while (consp l)
      (setq l (prog1 (cdr l)
                (rplacd l (prog1 prev
                            (setq prev l))))))
    prev))

(defmacro let* (binds . body)
  (cons (list 'lambda (map car binds)
              (cons 'progn
                    (nconc (map (lambda (b) (cons 'setq b)) binds)
                           body)))
        (map (lambda (x) nil) binds)))

(defmacro labels (binds . body)
  (cons (list 'lambda (map car binds)
              (cons 'progn
                    (nconc (map (lambda (b)
                                  (list 'setq (car b) (cons 'lambda (cdr b))))
                                binds)
                           body)))
        (map (lambda (x) nil) binds)))

(defmacro when   (c . body) (list 'if c (f-body body) nil))
(defmacro unless (c . body) (list 'if c nil (f-body body)))

(defmacro dotimes (var . body)
  (let ((v (car var))
        (cnt (cadr var)))
    `(for 0 (- ,cnt 1)
          (lambda (,v) ,(f-body body)))))

(defun map-int (f n)
  (if (<= n 0)
      ()
    (let ((first (cons (f 0) nil))
          (acc nil))
      (setq acc first)
      (for 1 (- n 1)
           (lambda (i)
             (progn (rplacd acc (cons (f i) nil))
                    (setq acc (cdr acc)))))
      first)))

(defun iota (n) (map-int identity n))
(define ι iota)

(defun error args (raise (cons 'error args)))

(defmacro throw (tag value) `(raise (list 'thrown-value ,tag ,value)))
(defmacro catch (tag expr)
  (let ((e (gensym)))
    `(trycatch ,expr
               (lambda (,e) (if (and (consp ,e)
                                     (eq (car  ,e) 'thrown-value)
                                     (eq (cadr ,e) ,tag))
                                (caddr ,e)
                              (raise ,e))))))

(defmacro unwind-protect (expr finally)
  (let ((e (gensym)))
    `(prog1 (trycatch ,expr
                      (lambda (,e) (progn ,finally (raise ,e))))
       ,finally)))

; (try expr
;      (catch (type-error e) . exprs)
;      (catch (io-error e) . exprs)
;      (catch (e) . exprs)
;      (finally . exprs))
(defmacro try (expr . forms)
  (let* ((e        (gensym))
         (reraised (gensym))
         (final (f-body (cdr (or (assoc 'finally forms) '(())))))
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
                                              (and (consp ,e)
                                                   (eq (car ,e)
                                                       ',extype)))
                                       T); (catch (e) ...), match anything
                                    (let ((,var ,e)) (progn ,@todo)))))
                              catches)
                       (T (raise ,e))))) ; no matches, reraise
    (if final
        (if catches
            ; form with both catch and finally
            `(prog1 (trycatch ,expr
                              (lambda (,e)
                                (trycatch ,catchblock
                                          (lambda (,reraised)
                                            (progn ,final
                                                   (raise ,reraised))))))
               ,final)
          ; finally only; same as unwind-protect
          `(prog1 (trycatch ,expr (lambda (,e)
                                    (progn ,final (raise ,e))))
             ,final))
      ; catch, no finally
      `(trycatch ,expr (lambda (,e) ,catchblock)))))

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
        (symbol-plist      set-symbol-plist   identity)
        (symbol-syntax     set-syntax         identity)))

(defun setf-place-mutator (place val)
  (if (symbolp place)
      (list 'setq place val)
    (let ((mutator (assoc (car place) *setf-place-list*)))
      (if (null mutator)
          (error '|setf: unknown place | (car place))
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

(defun list-to-vector (l) (apply vector l))
(defun vector-to-list (v)
  (let ((n (length v))
        (l nil))
    (for 1 n
         (lambda (i)
           (setq l (cons (aref v (- n i)) l))))
    l))

(defun self-evaluating-p (x)
  (or (and (atom x)
           (not (symbolp x)))
      (and (constantp x)
           (eq x (eval x)))))

; backquote
(defmacro backquote (x) (bq-process x))

(defun splice-form-p (x)
  (or (and (consp x) (or (eq (car x) '*comma-at*)
                         (eq (car x) '*comma-dot*)))
      (eq x '*comma*)))

(defun bq-process (x)
  (cond ((self-evaluating-p x)
         (if (vectorp x)
             (let ((body (bq-process (vector-to-list x))))
               (if (eq (car body) 'list)
                   (cons vector (cdr body))
                 (list apply vector body)))
           x))
        ((atom x)                     (list 'quote x))
        ((eq (car x) 'backquote)      (bq-process (bq-process (cadr x))))
        ((eq (car x) '*comma*)        (cadr x))
        ((not (any splice-form-p x))
         (let ((lc    (lastcdr x))
               (forms (map bq-bracket1 x)))
           (if (null lc)
               (cons 'list forms)
             (nconc (cons 'nlist* forms) (list (bq-process lc))))))
        (T (let ((p x) (q ()))
             (while (and (consp p)
                         (not (eq (car p) '*comma*)))
               (setq q (cons (bq-bracket (car p)) q))
               (setq p (cdr p)))
             (let ((forms
                    (cond ((consp p) (nreconc q (list (cadr p))))
                          ((null p)  (nreverse q))
                          (T         (nreconc q (list (bq-process p)))))))
               (if (null (cdr forms))
                   (car forms)
                 (cons 'nconc forms)))))))

(defun bq-bracket (x)
  (cond ((atom x)                   (list list (bq-process x)))
        ((eq (car x) '*comma*)      (list list (cadr x)))
        ((eq (car x) '*comma-at*)   (list 'copy-list (cadr x)))
        ((eq (car x) '*comma-dot*)  (cadr x))
        (T                          (list list (bq-process x)))))

; bracket without splicing
(defun bq-bracket1 (x)
  (if (and (consp x) (eq (car x) '*comma*))
      (cadr x)
    (bq-process x)))

(defmacro assert (expr) `(if ,expr T (raise '(assert-failed ,expr))))

(defmacro time (expr)
  (let ((t0 (gensym)))
    `(let ((,t0 (time.now)))
       (prog1
           ,expr
         (princ "Elapsed time: " (- (time.now) ,t0) " seconds\n")))))

(defun vector.map (f v)
  (let* ((n (length v))
         (nv (vector.alloc n)))
    (for 0 (- n 1)
         (lambda (i)
           (aset nv i (f (aref v i)))))
    nv))

(defun table.pairs (t)
  (table.foldl (lambda (k v z) (cons (cons k v) z))
               () t))
(defun table.keys (t)
  (table.foldl (lambda (k v z) (cons k z))
               () t))
(defun table.values (t)
  (table.foldl (lambda (k v z) (cons v z))
               () t))
(defun table.clone (t)
  (let ((nt (table)))
    (table.foldl (lambda (k v z) (put nt k v))
                 () t)
    nt))
