(defun cons3 (x y)
  (lambda (m) (funcall m x y)))
(defun divBy (x y count)
  (cond ((= 0 (mod y x)) (divBy x (/ y x) (+ count 1)))
	(t count)))
(defun car3 (x)
  (funcall x (lambda (x y) (divBy 2 (* (expt1 2 x) (expt1 3 y)) 0))))
(defun expt1 (x y)
  (if (= 0 y) 1
      (* x (expt1 x (- y 1)))))
