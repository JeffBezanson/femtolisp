; property lists. they really suck.
(setq *plists* nil)

(defun symbol-plist (sym)
  (cdr (or (assoc sym *plists*) '(()))))

(defun set-symbol-plist (sym lst)
  (let ((p (assoc sym *plists*)))
    (if (null p)  ; sym has no plist yet
        (setq *plists* (cons (cons sym lst) *plists*))
      (rplacd p lst))))

(defun get (sym prop)
  (let ((pl (symbol-plist sym)))
    (if pl
        (let ((pr (member prop pl)))
          (if pr (cadr pr) nil))
      nil)))

(defun put (sym prop val)
  (let ((p (assoc sym *plists*)))
    (if (null p)  ; sym has no plist yet
        (setq *plists* (cons (list sym prop val) *plists*))
      (let ((pr (member prop p)))
        (if (null pr)  ; sym doesn't have this property yet
            (rplacd p (cons prop (cons val (cdr p))))
          (rplaca (cdr pr) val)))))
  val)
