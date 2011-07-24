(defun strings-in-file (filename)
  (with-open-file (str (make-pathname :name filename) :direction :input)
    (list-of-strings str)))
(defun list-of-strings (str)
  (multiple-value-bind (x y) (read-line str nil 'eof)
      (if y nil
	  (cons x (list-of-strings str)))))