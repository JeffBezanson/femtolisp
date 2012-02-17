(define-macro (while- test . forms)
  `((label -loop- (lambda ()
                    (if ,test
                        (begin ,@forms
                               (-loop-))
                      ())))))

(define (tw)
  (set! i 0)
  (while (< i 10000000) (set! i (+ i 1))))

(define (tw2)
  (letrec ((loop (lambda ()
                   (if (< i 10000000)
                       (begin (set! i (+ i 1))
                              (loop))
		     ()))))
          (loop)))

#|
interpreter:
while: 1.82sec
macro: 2.98sec

compiler:
while: 0.72sec
macro: 1.24sec
|#
