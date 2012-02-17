; Terminating equal predicate
; by Jeff Bezanson
;
; This version only considers pairs and simple atoms.

; equal?, with bounded recursion. returns 0 if we suspect
; nontermination, otherwise #t or #f for the correct answer.
(define (bounded-equal a b N)
  (cond ((<= N 0) 0)
	((and (pair? a) (pair? b))
	 (let ((as
		(bounded-equal (car a) (car b) (- N 1))))
	   (if (number? as)
	       0
	       (and as
		    (bounded-equal (cdr a) (cdr b) (- N 1))))))
	(else (eq? a b))))

; union-find algorithm

; find equivalence class of a cons cell, or #f if not yet known
; the root of a class is a cons that is its own class
(define (class table key)
  (let ((c (hashtable-ref table key #f)))
    (if (or (not c) (eq? c key))
	c
	(class table c))))

; move a and b to the same equivalence class, given c and cb
; as the current values of (class table a) and (class table b)
; Note: this is not quite optimal. We blindly pick 'a' as the
; root of the new class, but we should pick whichever class is
; larger.
(define (union! table a b c cb)
  (let ((ca (if c c a)))
    (if cb
	(hashtable-set! table cb ca))
    (hashtable-set! table a ca)
    (hashtable-set! table b ca)))

; cyclic equal. first, attempt to compare a and b as best
; we can without recurring. if we can't prove them different,
; set them equal and move on.
(define (cyc-equal a b table)
  (cond ((eq? a b)  #t)
	((not (and (pair? a) (pair? b)))  (eq? a b))
	(else
	 (let ((aa (car a))  (da (cdr a))
	       (ab (car b))  (db (cdr b)))
	   (cond ((or (not (eq? (atom? aa) (atom? ab)))
		      (not (eq? (atom? da) (atom? db)))) #f)
		 ((and (atom? aa)
		       (not (eq? aa ab))) #f)
		 ((and (atom? da)
		       (not (eq? da db))) #f)
		 (else
		  (let ((ca (class table a))
			(cb (class table b)))
		    (if (and ca cb (eq? ca cb))
			#t
			(begin (union! table a b ca cb)
			       (and (cyc-equal aa ab table)
				    (cyc-equal da db table)))))))))))

(define (equal a b)
  (let ((guess (bounded-equal a b 2048)))
    (if (boolean? guess) guess
	(cyc-equal a b (make-eq-hashtable)))))
