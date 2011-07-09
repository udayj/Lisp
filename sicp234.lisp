(defun horner-eval (x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-coeff)
		(+ this-coeff (* x higher-coeff))) 0 coefficient-sequence))