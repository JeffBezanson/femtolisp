;;; "sort.scm" Defines: sorted?, merge, merge!, sort, sort!
;;; Author : Richard A. O'Keefe (based on Prolog code by D.H.D.Warren)
;;;
;;; This code is in the public domain.

;;; Updated: 11 June 1991
;;; Modified for scheme library: Aubrey Jaffer 19 Sept. 1991
;;; Updated: 19 June 1995
;;; (sort, sort!, sorted?): Generalized to strings by jaffer: 2003-09-09
;;; (sort, sort!, sorted?): Generalized to arrays by jaffer: 2003-10-04
;;; jaffer: 2006-10-08:
;;; (sort, sort!, sorted?, merge, merge!): Added optional KEY argument.
;;; jaffer: 2006-11-05:
;;; (sorted?, merge, merge!, sort, sort!): Call KEY arg at most once
;;; per element.

;(require 'array)

;;; (sorted? sequence less?)
;;; is true when sequence is a list (x0 x1 ... xm) or a vector #(x0 ... xm)
;;; such that for all 1 <= i <= m,
;;;     (not (less? (list-ref list i) (list-ref list (- i 1)))).
;@
(define (sorted? seq less? . opt-key)
  (define key (if (null? opt-key) identity (car opt-key)))
  (cond ((null? seq) #t)
	((array? seq)
	 (let ((dimax (+ -1 (car (array-dimensions seq)))))
	   (or (<= dimax 1)
	       (let loop ((idx (+ -1 dimax))
			  (last (key (array-ref seq dimax))))
		 (or (negative? idx)
		     (let ((nxt (key (array-ref seq idx))))
		       (and (less? nxt last)
			    (loop (+ -1 idx) nxt))))))))
	((null? (cdr seq)) #t)
	(else
	 (let loop ((last (key (car seq)))
		    (next (cdr seq)))
	   (or (null? next)
	       (let ((nxt (key (car next))))
		 (and (not (less? nxt last))
		      (loop nxt (cdr next)))))))))

;;; (merge a b less?)
;;; takes two lists a and b such that (sorted? a less?) and (sorted? b less?)
;;; and returns a new list in which the elements of a and b have been stably
;;; interleaved so that (sorted? (merge a b less?) less?).
;;; Note:  this does _not_ accept arrays.  See below.
;@
(define (merge a b less? . opt-key)
  (define key (if (null? opt-key) identity (car opt-key)))
  (cond ((null? a) b)
	((null? b) a)
	(else
	 (let loop ((x (car a)) (kx (key (car a))) (a (cdr a))
		    (y (car b)) (ky (key (car b))) (b (cdr b)))
	   ;; The loop handles the merging of non-empty lists.  It has
	   ;; been written this way to save testing and car/cdring.
	   (if (less? ky kx)
	       (if (null? b)
		   (cons y (cons x a))
		   (cons y (loop x kx a (car b) (key (car b)) (cdr b))))
	       ;; x <= y
	       (if (null? a)
		   (cons x (cons y b))
		   (cons x (loop (car a) (key (car a)) (cdr a) y ky b))))))))

(define (sort:merge! a b less? key)
  (define (loop r a kcara b kcarb)
    (cond ((less? kcarb kcara)
	   (set-cdr! r b)
	   (if (null? (cdr b))
	       (set-cdr! b a)
	       (loop b a kcara (cdr b) (key (cadr b)))))
	  (else				; (car a) <= (car b)
	   (set-cdr! r a)
	   (if (null? (cdr a))
	       (set-cdr! a b)
	       (loop a (cdr a) (key (cadr a)) b kcarb)))))
  (cond ((null? a) b)
	((null? b) a)
	(else
	 (let ((kcara (key (car a)))
	       (kcarb (key (car b))))
	   (cond
	    ((less? kcarb kcara)
	     (if (null? (cdr b))
		 (set-cdr! b a)
		 (loop b a kcara (cdr b) (key (cadr b))))
	     b)
	    (else			; (car a) <= (car b)
	     (if (null? (cdr a))
		 (set-cdr! a b)
		 (loop a (cdr a) (key (cadr a)) b kcarb))
	     a))))))

;;; takes two sorted lists a and b and smashes their cdr fields to form a
;;; single sorted list including the elements of both.
;;; Note:  this does _not_ accept arrays.
;@
(define (merge! a b less? . opt-key)
  (sort:merge! a b less? (if (null? opt-key) identity (car opt-key))))

(define (sort:sort-list! seq less? key)
  (define keyer (if key car identity))
  (define (step n)
    (cond ((> n 2) (let* ((j (quotient n 2))
			  (a (step j))
			  (k (- n j))
			  (b (step k)))
		     (sort:merge! a b less? keyer)))
	  ((= n 2) (let ((x (car seq))
			 (y (cadr seq))
			 (p seq))
		     (set! seq (cddr seq))
		     (cond ((less? (keyer y) (keyer x))
			    (set-car! p y)
			    (set-car! (cdr p) x)))
		     (set-cdr! (cdr p) '())
		     p))
	  ((= n 1) (let ((p seq))
		     (set! seq (cdr seq))
		     (set-cdr! p '())
		     p))
	  (else '())))
  (define (key-wrap! lst)
    (cond ((null? lst))
	  (else (set-car! lst (cons (key (car lst)) (car lst)))
		(key-wrap! (cdr lst)))))
  (define (key-unwrap! lst)
    (cond ((null? lst))
	  (else (set-car! lst (cdar lst))
		(key-unwrap! (cdr lst)))))
  (cond (key
	 (key-wrap! seq)
	 (set! seq (step (length seq)))
	 (key-unwrap! seq)
	 seq)
	(else
	 (step (length seq)))))

(define (rank-1-array->list array)
  (define dimensions (array-dimensions array))
  (do ((idx (+ -1 (car dimensions)) (+ -1 idx))
       (lst '() (cons (array-ref array idx) lst)))
      ((< idx 0) lst)))

;;; (sort! sequence less?)
;;; sorts the list, array, or string sequence destructively.  It uses
;;; a version of merge-sort invented, to the best of my knowledge, by
;;; David H. D.  Warren, and first used in the DEC-10 Prolog system.
;;; R. A. O'Keefe adapted it to work destructively in Scheme.
;;; A. Jaffer modified to always return the original list.
;@
(define (sort! seq less? . opt-key)
  (define key (if (null? opt-key) #f (car opt-key)))
  (cond ((array? seq)
	 (let ((dims (array-dimensions seq)))
	   (do ((sorted (sort:sort-list! (rank-1-array->list seq) less? key)
			(cdr sorted))
		(i 0 (+ i 1)))
	       ((null? sorted) seq)
	     (array-set! seq (car sorted) i))))
	(else			      ; otherwise, assume it is a list
	 (let ((ret (sort:sort-list! seq less? key)))
	   (if (not (eq? ret seq))
	       (do ((crt ret (cdr crt)))
		   ((eq? (cdr crt) seq)
		    (set-cdr! crt ret)
		    (let ((scar (car seq)) (scdr (cdr seq)))
		      (set-car! seq (car ret)) (set-cdr! seq (cdr ret))
		      (set-car! ret scar) (set-cdr! ret scdr)))))
	   seq))))

;;; (sort sequence less?)
;;; sorts a array, string, or list non-destructively.  It does this
;;; by sorting a copy of the sequence.  My understanding is that the
;;; Standard says that the result of append is always "newly
;;; allocated" except for sharing structure with "the last argument",
;;; so (append x '()) ought to be a standard way of copying a list x.
;@
(define (sort seq less? . opt-key)
  (define key (if (null? opt-key) #f (car opt-key)))
  (cond ((array? seq)
	 (let ((dims (array-dimensions seq)))
	   (define newra (apply make-array seq dims))
	   (do ((sorted (sort:sort-list! (rank-1-array->list seq) less? key)
			(cdr sorted))
		(i 0 (+ i 1)))
	       ((null? sorted) newra)
	     (array-set! newra (car sorted) i))))
	(else (sort:sort-list! (append seq '()) less? key))))
