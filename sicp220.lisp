(defun same-parity (&rest values)
  (let ((x (mod (car values) 2)))
  (mapcan (lambda (y)  (if (= x (mod y 2)) (list y) nil)) values)))
		       