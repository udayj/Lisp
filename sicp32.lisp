(defun make-monitored (procedure)
  (let ((counter 0))
  (lambda (arg)
    (cond ((equalp arg 'how-many-calls?) counter)
	  ((equalp arg 'reset-counter) (progn 
					 (setf counter 0)
					 counter))
	  (t (progn
	       (incf counter)
	       (funcall procedure arg)))))))
(defun square (x)
  (* x x))