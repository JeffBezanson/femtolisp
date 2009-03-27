; -*- scheme -*-

(define (rule30-step b)
  (let ((L (ash b -1))
	(R (ash b 1)))
    (let ((~b (lognot b))
	  (~L (lognot L))
	  (~R (lognot R)))
      (logior (logand  L ~b ~R)
	      (logand ~L  b  R)
	      (logand ~L  b ~R)
	      (logand ~L ~b  R)))))

(define (nestlist f zero n)
  (if (<= n 0) ()
      (cons zero (nestlist f (f zero) (- n 1)))))

(define (string.rep s k)
  (cond ((< k 4)
	 (cond ((<= k 0) "")
	       ((=  k 1) (string s))
	       ((=  k 2) (string s s))
	       (else     (string s s s))))
	((odd? k) (string s (string.rep s (- k 1))))
	(else     (string.rep (string s s) (/ k 2)))))

(define (pad0 s n) (string (string.rep "0" (- n (length s))) s))

(define (bin-draw s)
  (string.map (lambda (c) (case c
			    (#\1 #\#)
			    (#\0 #\ )
			    (else c)))
	      s))

(for-each (lambda (n)
	    (begin
	      (princ (bin-draw (pad0 (number->string n 2) 63)))
	      (newline)))
	  (nestlist rule30-step (uint64 0x0000000080000000) 32))
