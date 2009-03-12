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

(define (make-string k ch)
  (cond ((<= k 0) "")
	((=  k 1) (string ch))
	((=  k 2) (string ch ch))
	((odd? k) (string ch (make-string (- k 1) ch)))
	(else (let ((half (make-string (/ k 2) ch)))
		(string half half)))))

(define (pad0 s n) (string (make-string (- n (length s)) "0") s))

(define (bin-draw s)
  (string.map (lambda (c) (case c
			    (#\1 #\#)
			    (#\0 #\ )
			    (else c)))
	      s))

(for-each (lambda (n)
	    (begin
	      (princ (bin-draw (pad0 (number->string n 2) 63)))
	      (terpri)))
	  (nestlist rule30-step (uint64 0x0000000080000000) 32))
