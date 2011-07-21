(defun max-min (v pos max min)
  (if (= pos (length v))
      (values max min)
      (let ((num (aref v pos)))
	(max-min v (+ pos 1) 
		 (if (> num max)
		     num
		     max)
		 (if (< num min)
		     num
		     min)))))
		   