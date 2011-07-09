(defun cons1 (x y)
  (lambda (m) (funcall m x y)))
(defun car1 (x)
  (funcall x (lambda (p q) p)))