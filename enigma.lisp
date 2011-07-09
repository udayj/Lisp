(defun enigma (x)
  (and (not (null x))
       (or (null (cdr x))
	   (enigma (cdr x)))))