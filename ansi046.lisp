(defun assoc-to-hash (l)
  (let ((ret (make-hash-table)))
    (progn
      (mapcar (lambda (x) (setf (gethash (car x) ret) (cdr x))) 
	    l)
      ret)))
(defun hash-to-assoc (l)
  (let ((ret nil))
    (progn
      (maphash (lambda (k v) (setf ret (cons (cons k v) ret)))
	       l)
      ret)))