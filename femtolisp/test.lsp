; -*- scheme -*-

; make label self-evaluating, but evaluating the lambda in the process
;(defmacro labl (name f)
;  (list list ''labl (list 'quote name) f))

(define-macro (labl name f)
  `(let (,name) (set! ,name ,f)))

;(define (reverse lst)
;  ((label rev-help (lambda (lst result)
;                     (if (null? lst) result
;                       (rev-help (cdr lst) (cons (car lst) result)))))
;   lst ()))

(define (append- . lsts)
  ((label append-h
          (lambda (lsts)
            (cond ((null? lsts) ())
                  ((null? (cdr lsts)) (car lsts))
                  (#t ((label append2 (lambda (l d)
					(if (null? l) d
					    (cons (car l)
						  (append2 (cdr l) d)))))
		       (car lsts) (append-h (cdr lsts)))))))
   lsts))

;(princ 'Hello '| | 'world! "\n")
;(filter (lambda (x) (not (< x 0))) '(1 -1 -2 5 10 -8 0))
(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
;(princ (time (fib 34)) "\n")
;(dotimes (i 20000) (map-int (lambda (x) (list 'quote x)) 8))
;(dotimes (i 40000) (append '(a b) '(1 2 3 4) () '(c) () '(5 6)))
;(dotimes (i 80000) (list 1 2 3 4 5))
;(set! a (map-int identity 10000))
;(dotimes (i 200) (rfoldl cons () a))

(define (sort l)
  (if (or (null? l) (null? (cdr l))) l
    (let* ((piv (car l))
           (halves (separate (lambda (x) (< x piv)) (cdr l))))
      (nconc (sort (car halves))
             (list piv)
             (sort (cdr halves))))))

#|
(define-macro (dotimes var . body)
  (let ((v   (car var))
        (cnt (cadr var)))
    `(let ((,v 0))
       (while (< ,v ,cnt)
         (prog1
             ,(cons 'begin body)
           (set! ,v (+ ,v 1)))))))

(define (map-int f n)
  (if (<= n 0)
      ()
      (let ((first (cons (f 0) ())))
	((label map-int-
		(lambda (acc i n)
		  (if (= i n)
		      first
		      (begin (set-cdr! acc (cons (f i) ()))
			     (map-int- (cdr acc) (+ i 1) n)))))
	 first 1 n))))
|#

(define-macro (labl name fn)
  `((lambda (,name) (set! ,name ,fn)) ()))

(define (square x) (* x x))
(define (expt b p)
  (cond ((= p 0) 1)
        ((= b 0) 0)
        ((even? p) (square (expt b (div0 p 2))))
        (#t (* b (expt b (- p 1))))))

(define (gcd a b)
  (cond ((= a 0) b)
        ((= b 0) a)
        ((< a b)  (gcd a (- b a)))
        (#t       (gcd b (- a b)))))

; like eval-when-compile
(define-macro (literal expr)
  (let ((v (eval expr)))
    (if (self-evaluating? v) v (list quote v))))

(define (cardepth l)
  (if (atom? l) 0
      (+ 1 (cardepth (car l)))))

(define (nestlist f zero n)
  (if (<= n 0) ()
      (cons zero (nestlist f (f zero) (- n 1)))))

(define (mapl f . lsts)
  ((label mapl-
          (lambda (lsts)
            (if (null? (car lsts)) ()
		(begin (apply f lsts) (mapl- (map cdr lsts))))))
   lsts))

; test to see if a symbol begins with :
(define (keywordp s)
  (and (>= s '|:|) (<= s '|:~|)))

; swap the cars and cdrs of every cons in a structure
(define (swapad c)
  (if (atom? c) c
      (set-cdr! c (K (swapad (car c))
		     (set-car! c (swapad (cdr c)))))))

(define (without x l)
  (filter (lambda (e) (not (eq e x))) l))

(define (conscount c)
  (if (pair? c) (+ 1
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

(define-macro (while- test . forms)
  `((label -loop- (lambda ()
                    (if ,test
                        (begin ,@forms
                               (-loop-))
			())))))

; this would be a cool use of thunking to handle 'finally' clauses, but
; this code doesn't work in the case where the user manually re-raises
; inside a catch block. one way to handle it would be to replace all
; their uses of 'raise' with '*_try_finally_raise_*' which calls the thunk.
; (try expr
;      (catch (TypeError e) . exprs)
;      (catch (IOError e) . exprs)
;      (finally . exprs))
(define-macro (try expr . forms)
  (let ((final (f-body (cdr (or (assq 'finally forms) '(())))))
        (body (foldr
               ; create a function to check for and handle one exception
               ; type, and pass off control to the next when no match
               (lambda (catc next)
                 (let ((var    (cadr (cadr catc)))
                       (extype (caadr catc))
                       (todo   (f-body (cddr  catc))))
                   `(lambda (,var)
                      (if (or (eq ,var ',extype)
                              (and (pair? ,var)
                                   (eq (car ,var) ',extype)))
                          ,todo
                        (,next ,var)))))

               ; default function; no matches so re-raise
               '(lambda (e) (begin (*_try_finally_thunk_*) (raise e)))

               ; make list of catch forms
               (filter (lambda (f) (eq (car f) 'catch)) forms))))
    `(let ((*_try_finally_thunk_* (lambda () ,final)))
       (prog1 (attempt ,expr ,body)
         (*_try_finally_thunk_*)))))

(define Y
  (lambda (f)
    ((lambda (h)
       (f (lambda (x) ((h h) x))))
     (lambda (h)
       (f (lambda (x) ((h h) x)))))))

(define yfib
  (Y (lambda (fib)
       (lambda (n)
         (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))))

;(defun tt () (time (dotimes (i 500000) (* 0x1fffffff 1) )))
;(tt)
;(tt)
;(tt)

(define-macro (accumulate-while cnd what . body)
  (let ((first (gensym))
        (acc   (gensym)))
    `(let ((,first ())
           (,acc (list ())))
       (set! ,first ,acc)
       (while ,cnd
	      (begin (set! ,acc
			   (cdr (set-cdr! ,acc (cons ,what ()))))
		     ,@body))
       (cdr ,first))))

(define-macro (accumulate-for var lo hi what . body)
  (let ((first (gensym))
        (acc   (gensym)))
    `(let ((,first ())
           (,acc (list ())))
       (set! ,first ,acc)
       (for ,lo ,hi
            (lambda (,var)
              (begin (set! ,acc
                           (cdr (set-cdr! ,acc (cons ,what ()))))
                     ,@body)))
       (cdr ,first))))

(define (map-indexed f lst)
  (if (atom? lst) lst
    (let ((i 0))
      (accumulate-while (pair? lst) (f (car lst) i)
                        (begin (set! lst (cdr lst))
                               (set! i (1+ i)))))))

(define (string.findall haystack needle . offs)
  (define (sub h n offs lst)
    (let ((i (string.find h n offs)))
      (if i
	  (sub h n (string.inc h i) (cons i lst))
	  (reverse! lst))))
  (sub haystack needle (if (null? offs) 0 (car offs)) ()))
