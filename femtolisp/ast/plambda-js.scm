; pattern-lambda syntax for jscheme

; pattern-lambda abstraction
; this is a generalization of lambda:
;
; ((pattern-lambda p body) expr)
; Matches expr against p. If no match, return #null. If match succeeds, evaluate body
; with variables in p bound to whatever they matched in expr.
;
; EXAMPLE: Recognize adding any expression x to itself, replace with 2*x.
; (define selfadd (pattern-lambda (+ x x) `(* 2 ,x)))
; Then (selfadd '(+ (foo bar) (foo bar))) returns (* 2 (foo bar))
;
(define-macro (pattern-lambda pat body)
  (let* ((args (patargs pat))
	 (expander `(lambda ,args ,body)))
    `(lambda (expr)
       (let ((m (match ',pat expr)))
	 (if m
	     ; matches; perform expansion
	     (apply ,expander (map (lambda (var) (cdr (or (assq var m) '(0 . #f))))
				   ',args))
	     #f)))))
