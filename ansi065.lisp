(defun remove-if-custom (fn lst)
  (filter (lambda (x) (not (funcall fn x))) lst))
(defun filter (fn lst)
  (if (null lst) nil
      (if (funcall fn (car lst))
	  (cons (car lst) (filter fn (cdr lst)))
	  (filter fn (cdr lst)))))