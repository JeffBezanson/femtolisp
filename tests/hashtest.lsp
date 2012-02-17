; -*- scheme -*-

(define (hins1)
  (let ((h (table)))
    (dotimes (n 200000)
      (put! h (mod (rand) 1000) 'apple))
    h))

(define (hread h)
  (dotimes (n 200000)
    (get h (mod (rand) 10000) nil)))

(time (dotimes (i 100000)
        (table :a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7 :foo 8 :bar 9)))
(time (dotimes (i 100000) (table :a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7 :foo 8)))
(time (dotimes (i 100000) (table :a 1 :b 2 :c 3 :d 4)))
(time (dotimes (i 100000) (table :a 1 :b 2)))
(time (dotimes (i 100000) (table)))

#t

#|

with HT_N_INLINE==16
Elapsed time: 0.0796329975128174 seconds
Elapsed time: 0.0455679893493652 seconds
Elapsed time: 0.0272290706634521 seconds
Elapsed time: 0.0177979469299316 seconds
Elapsed time: 0.0102229118347168 seconds


with HT_N_INLINE==8

Elapsed time: 0.1010119915008545 seconds
Elapsed time: 0.174872875213623 seconds
Elapsed time: 0.0322129726409912 seconds
Elapsed time: 0.0195930004119873 seconds
Elapsed time: 0.008836030960083 seconds

|#
