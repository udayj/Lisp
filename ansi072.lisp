(defun expressions-in-file (filename)
  (with-open-file (str (make-pathname :name filename) :direction :input)
    (list-of-expressions str)))
(defun list-of-expressions (str)
  (multiple-value-bind (x) (read str nil 'eof)
      (if (eq x 'eof) nil
	  (cons x (list-of-expressions str)))))