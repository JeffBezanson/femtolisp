(setq i 0)
(defmacro while- (test . forms)
  `((label -loop- (lambda ()
                    (if ,test
                        (progn ,@forms
                               (-loop-))
                      nil)))))
(while (< i 10000000) (setq i (+ i 1)))
