; utilities for AST processing

(define (symconcat s1 s2)
  (intern (string s1 s2)))

(define (list-adjoin item lst)
  (if (member item lst)
      lst
    (cons item lst)))

(define (index-of item lst start)
  (cond ((null lst) nil)
	((eq item (car lst)) start)
	(T (index-of item (cdr lst) (+ start 1)))))

(define (each f l)
  (if (null l) l
    (progn (f (car l))
           (each f (cdr l)))))

(define (maptree-pre f tr)
  (let ((new-t (f tr)))
    (if (consp new-t)
        (map (lambda (e) (maptree-pre f e)) new-t)
      new-t)))

(define (maptree-post f tr)
  (if (not (consp tr))
      (f tr)
    (let ((new-t (map (lambda (e) (maptree-post f e)) tr)))
      (f new-t))))

; collapse forms like (&& (&& (&& (&& a b) c) d) e) to (&& a b c d e)
(define (flatten-left-op op e)
  (maptree-post (lambda (node)
                  (if (and (consp node)
                           (eq (car node) op)
                           (consp (cdr node))
                           (consp (cadr node))
                           (eq (caadr node) op))
                      (cons op
                            (append (cdadr node) (cddr node)))
                    node))
                e))

; convert all local variable references to (lexref rib slot name)
; where rib is the nesting level and slot is the stack slot#
; name is just there for reference
; this assumes lambda is the only remaining naming form
(define (lookup-var v env lev)
  (if (null env) v
    (let ((i (index-of v (car env) 0)))
      (if i (list 'lexref lev i v)
        (lookup-var v (cdr env) (+ lev 1))))))
(define (lvc- e env)
  (cond ((symbolp e) (lookup-var e env 0))
        ((consp e)
         (if (eq (car e) 'quote)
             e
           (let* ((newvs (and (eq (car e) 'lambda) (cadr e)))
                  (newenv (if newvs (cons newvs env) env)))
             (if newvs
                 (cons 'lambda
                       (cons (cadr e)
                             (map (lambda (se) (lvc- se newenv))
                                  (cddr e))))
               (map (lambda (se) (lvc- se env)) e)))))
        (T e)))
(define (lexical-var-conversion e)
  (lvc- e ()))

; convert let to lambda
(define (let-expand e)
  (maptree-post (lambda (n)
		  (if (and (consp n) (eq (car n) 'let))
		      `((lambda ,(map car (cadr n)) ,@(cddr n))
			,@(map cadr (cadr n)))
                    n))
		e))

; flatten op with any associativity
(defmacro flatten-all-op (op e)
  `(pattern-expand
    (pattern-lambda (,op (-- l ...) (-- inner (,op ...)) (-- r ...))
                    (cons ',op (append l (cdr inner) r)))
    ,e))

(defmacro pattern-lambda (pat body)
  (let* ((args (patargs pat))
         (expander `(lambda ,args ,body)))
    `(lambda (expr)
       (let ((m (match ',pat expr)))
         (if m
             ; matches; perform expansion
             (apply ,expander (map (lambda (var) (cdr (or (assoc var m) '(0 . nil))))
                                   ',args))
           nil)))))
