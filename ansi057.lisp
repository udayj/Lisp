(defun successive-diff (x)
  (block outer
    (mapc (lambda (x y)
	    (if (not (or 
		 (= (- x y) 1)
		 (= (- y x) 1)))
		(return-from outer nil)))
	  x
	  (append (cdr x) (list (+ (car (last x)) 1))))
    t))