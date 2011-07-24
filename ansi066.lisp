(defun make-max-so-far ()
  (let ((start 0)
	(max 0))
    (lambda (x)
      (if (= start 0) 
	  (progn
	    (setf start 1)
	    (setf max x)
	    x)
	  (if (> x max)
	      (progn
		(setf max x)
		x)
	      max)))))
	      
  