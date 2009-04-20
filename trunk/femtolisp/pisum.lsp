(define (pisum)
  (dotimes (j 500)
    ((label sumloop
            (lambda (i sum)
              (if (> i 10000)
                  sum
                (sumloop (+ i 1) (+ sum (/ (* i i)))))))
     1.0 0.0)))
