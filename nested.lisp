(defun nested (n)
  (accumulate #'append nil (map_alt
		   (lambda (i)
		     (map_alt (lambda (j) (list i j)) 
			  (enumerate-interval 1 (- i 1)))) 
		     (enumerate-interval 1 n))))
(defun enumerate-interval (i j)
  (if (> i j) nil
      (cons i (enumerate-interval (+ i 1) j))))