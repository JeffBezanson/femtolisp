(set! i 0)
(define-macro (while- test . forms)
  `((label -loop- (lambda ()
                    (if ,test
                        (begin ,@forms
                               (-loop-))
			nil)))))
(while (< i 10000000) (set! i (+ i 1)))
