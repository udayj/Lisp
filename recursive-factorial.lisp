(defun fact(num)
  (if (<= num 1)
     num
  (setf factorial (* num (fact (- num 1))))))