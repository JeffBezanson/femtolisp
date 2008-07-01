(define (equal a b)
  (if (and (consp a) (consp b))
      (and (equal (car a) (car b))
           (equal (cdr a) (cdr b)))
    (eq a b)))

; compare imposes an ordering on all values. yields -1 for a<b,
; 0 for a==b, and 1 for a>b. lists are compared up to the first
; point of difference.
(defun compare (a b)
  (cond ((eq a b) 0)
        ((or (atom a) (atom b)) (if (< a b) -1 1))
        (T (let ((c (compare (car a) (car b))))
             (if (not (eq c 0))
                 c
               (compare (cdr a) (cdr b)))))))

(defun length (l)
  (if (null l) 0
    (+ 1 (length (cdr l)))))

(define (assoc item lst)
  (cond ((atom lst) ())
        ((eq (caar lst) item) (car lst))
        (T (assoc item (cdr lst)))))
