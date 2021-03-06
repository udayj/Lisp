(defun infinite? (x)
  (cond ((nil? x) nil)
	((nil? (cdr x)) nil)
	(t (traverse (cdr x) (cddr x)))))
(defun traverse (x y)
  (cond ((or (nil? x) (nil? y) (nil? (cdr y))) nil)
	((eq x y ) t)
	(t (traverse (cdr x) (cddr y)))))