(defun func (f n a)
  (cond ((= n 1) (funcall f a))
	(t (func f (- n 1) (funcall f a)))))
(defun sq (a)
  (* a a))