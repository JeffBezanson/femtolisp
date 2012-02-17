(let ((t (table)))
  (time (dotimes (i 2000000)
          (put! t (rand) (rand)))))
#t
