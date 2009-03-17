; -*- scheme -*-
; (try expr
;      (catch (type-error e) . exprs)
;      (catch (io-error e) . exprs)
;      (catch (e) . exprs)
;      (finally . exprs))
(define-macro (try expr . forms)
  (let* ((e        (gensym))
         (reraised (gensym))
         (final (f-body (cdr (or (assq 'finally forms) '(())))))
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
                                              (and (pair? ,e)
                                                   (eq (car ,e)
                                                       ',extype)))
					 #t); (catch (e) ...), match anything
                                    (let ((,var ,e)) (begin ,@todo)))))
                              catches)
                       (#t (raise ,e))))) ; no matches, reraise
    (if final
        (if catches
            ; form with both catch and finally
            `(prog1 (trycatch ,expr
                              (lambda (,e)
                                (trycatch ,catchblock
                                          (lambda (,reraised)
                                            (begin ,final
                                                   (raise ,reraised))))))
               ,final)
          ; finally only; same as unwind-protect
          `(prog1 (trycatch ,expr (lambda (,e)
                                    (begin ,final (raise ,e))))
             ,final))
      ; catch, no finally
      `(trycatch ,expr (lambda (,e) ,catchblock)))))

; setf
; expands (setf (place x ...) v) to (mutator (f x ...) v)
; (mutator (identity x ...) v) is interpreted as (mutator x ... v)
(set! *setf-place-list*
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
        (list-ref rplaca  nthcdr)
        (get     put!     identity)
        (aref    aset!    identity)
        (symbol-syntax    set-syntax!        identity)))

(define (setf-place-mutator place val)
  (if (symbol? place)
      (list 'set! place val)
    (let ((mutator (assq (car place) *setf-place-list*)))
      (if (null? mutator)
          (error "setf: unknown place " (car place))
	  (if (eq (caddr mutator) 'identity)
	      (cons (cadr mutator) (append (cdr place) (list val)))
	      (list (cadr mutator)
		    (cons (caddr mutator) (cdr place))
		    val))))))

(define-macro (setf . args)
  (f-body
   ((label setf-
           (lambda (args)
             (if (null? args)
                 ()
               (cons (setf-place-mutator (car args) (cadr args))
                     (setf- (cddr args))))))
    args)))

(define-macro (labels binds . body)
  (cons (list 'lambda (map car binds)
              (f-body
	       (nconc (map (lambda (b)
			     (list 'set! (car b) (cons 'lambda (cdr b))))
			   binds)
		      body)))
        (map (lambda (x) #f) binds)))

  (define (evalhead e env)
    (if (and (symbol? e)
	     (or (constant? e)
		 (and (not (memq e env))
		      (bound? e)
		      (builtin? (eval e)))))
	(eval e)
	e))
