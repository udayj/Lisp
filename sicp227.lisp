
(defun reverse1 (x)
  (cond ((nil? x) nil)
	((atom x) x)
	(t (append (reverse1 (cdr x)) (list (car x))))))
(defun deep-reverse (x)
  (if (nil? x) nil
      (append (deep-reverse (cdr x)) (list (reverse1 (car x))))))
(defun nil? (x)
  (equalp nil x))
