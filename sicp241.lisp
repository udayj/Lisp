(defun ordered (n s)
  (filter 
   (lambda (x) (unique-sum x s)) 
   (accumulate #'append nil (map_alt (lambda (i)
	     (flatmap (lambda (j) (map_alt 
				   (lambda (k)
				     (cond ((and (/= i j) (/= i k) (/= j k))
					    (list i j k))
					   (t (list i j k))
					   )) 
				   (enumerate-interval 1 n)))
		      (enumerate-interval 1 n)))
		      (enumerate-interval 1 n)))))

(defun unique-sum (x s)
  (let ((i (car x))
	(j (car (cdr x)))
	(k (car (cdr (cdr x)))))
    (and (/= i j) (/= i k) (/= j k) (= s (+ i j k)))))
(defun enumerate-interval (low high)
  (if (> low high) nil
      (cons low (enumerate-interval (+ low 1) high))))
(defun nil? (x)
  (equalp nil x))
	
