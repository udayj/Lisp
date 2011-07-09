(defun double (proc)
  (lambda (x) (funcall proc (funcall proc x))))
(defun inc (x)
  (+ x 1))