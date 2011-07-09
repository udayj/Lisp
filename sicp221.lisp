(defun square (x)
  (* x x))
(defun square-list (items)
  (if (nil? items) nil
      (append (list (square (first items))) (square-list (rest items)))))
(defun nil? (x)
  (equalp nil x))
  