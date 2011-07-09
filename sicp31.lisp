(defun make-accumulator (sum)
  (lambda (amount)
    (progn 
      (setf sum (+ sum amount))
      sum)))