(defun f (x)
  (if (= *variable* 1)
      (progn
	(setf *variable* (+ *variable* 1))
	x)
      0))
      
      