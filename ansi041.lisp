(defun rotate (arr i j n)
  (if (= i n) arr
      (if (= j n)
	  (rotate arr (+ i 1) (+ i 1) n)
          (progn
	    (swap arr i j n)
	    (rotate arr i (+ j 1) n)))))
(defun swap (arr x y n)
  (let ((temp (aref arr x y)))
    (setf (aref arr x y) (aref arr (- n y 1) x))
    (setf (aref arr (- n y 1) x) temp)))
	  