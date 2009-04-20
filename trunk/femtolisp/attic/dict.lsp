; dictionary as binary tree

(defun dict () ())

; node representation ((k . v) L R)
(defun dict-peek (d key nf)
  (if (null d) nf
    (let ((c (compare key (caar d))))
      (cond ((= c 0) (cdar d))
            ((< c 0) (dict-peek (cadr  d) key nf))
            (T       (dict-peek (caddr d) key nf))))))

(defun dict-get (d key) (dict-peek d key nil))

(defun dict-put (d key v)
  (if (null d) (list (cons key v) (dict) (dict))
    (let ((c (compare key (caar d))))
      (cond ((= c 0) (list (cons key v) (cadr d) (caddr d)))
            ((< c 0) (list (car d)
                           (dict-put (cadr d) key v)
                           (caddr d)))
            (T       (list (car d)
                           (cadr d)
                           (dict-put (caddr d) key v)))))))

; mutable dictionary
(defun dict-nput (d key v)
  (if (null d) (list (cons key v) (dict) (dict))
    (let ((c (compare key (caar d))))
      (cond ((= c 0) (rplacd (car d) v))
            ((< c 0) (setf (cadr  d) (dict-nput (cadr  d) key v)))
            (T       (setf (caddr d) (dict-nput (caddr d) key v))))
      d)))

(defun dict-collect (f d)
  (if (null d) ()
    (cons (f (caar d) (cdar d)) (nconc (dict-collect f (cadr  d))
                                       (dict-collect f (caddr d))))))

(defun dict-keys  (d) (dict-collect K    d))
(defun dict-pairs (d) (dict-collect cons d))

(defun dict-each (f d)
  (if (null d) ()
    (progn (f (caar d) (cdar d))
           (dict-each f (cadr  d))
           (dict-each f (caddr d)))))

(defun alist-to-dict (a)
  (foldl (lambda (p d) (dict-put d (car p) (cdr p)))
         (dict) a))
