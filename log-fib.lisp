(defun fib (n)
  (fib-iter 1 0 0 1 n))
(defun fib-iter (a b p q count)
  (cond ((= 0 count) b)
	((even? count) (fib-iter
			a b (change-p p q) (change-q p q) (/ count 2)))
	(t (fib-iter (+ (* a (+ p q)) (* b q)) (+ (* b p) (* a q)) p q 
		     (- count 1)))))
(defun even? (n)
  (= 0 (mod n 2)))
(defun change-p (p q)
  (+ (* p p) (* q q)))
(defun change-q (p q)
  (+ (* q q) (* 2 p q)))