(defun list-equal? (x y)
  (let ((x1 (car x))
	(y1 (car y)))
    (if (and (nil? x) (nil? y)) t
    (if (not (or (and (consp x1) (consp y1)) (and (not (consp x1)) (not (consp y1))))) nil
	(if (not (consp x1)) (and (equalp x1 y1) (list-equal? (cdr x) (cdr y)))
	    (list-equal? (cdr x) (cdr y)))))))