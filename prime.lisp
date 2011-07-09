(defun prime?(n)
  (= n (prime n 2)))
(defun find-divisor (n x)
  (cond ((> (* x x) n) n)
	((= 0 (mod n x)) x)
	(t (prime n (+ x 1)))))
  