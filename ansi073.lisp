(defun remove-comments (file1 file2)
  (with-open-file (stri (make-pathname :name file1) :direction :input)
    (with-open-file (stro (make-pathname :name file2) 
			  :direction :output
			  :if-exists :supersede)
      (print-to-output (lines-without-comments stri) stro))))
(defun print-to-output (lst stro)
  (if (null lst) nil
      (progn
	(format stro "~A~%" (car lst))
	(print-to-output (cdr lst) stro))))
(defun lines-without-comments (stri)
  (multiple-value-bind (x y) (read-line stri nil 'eof)
    (cond (y nil)
	  ((and (> (length x) 0) (eq (char x 0) #\%))
	   (lines-without-comments stri))
	  (t
	   (cons x (lines-without-comments stri))))))