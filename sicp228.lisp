(defun fringe (items)
  (cond ((nil? items) nil)
	((consp items) (append (fringe (car items)) (fringe (cdr items))))
	(t (list items))))
(defun nil? (x)
  (equalp nil x))

