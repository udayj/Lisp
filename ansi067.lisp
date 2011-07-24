(defun make-max-prev ()
  (let ((start 0)
	(prev 0))
    (lambda (x)
      (if (= start 0) 
	  (progn
	    (setf start 1)
	    (setf prev x)
	    nil)
	  (if (> x prev)
	      (progn
		(setf prev x)
		t)
	      (progn 
		(setf prev x)
		nil))))))
	      
  