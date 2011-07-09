(defun make-table-l (samekey?)
  (let* ((table (list 'table))
	(lookup-l (defun lookup-temp (key1 key2)
		    (let ((subtable (assoc-l samekey? key1 (cdr table))))
		      (if subtable
			  (let ((record (assoc-l samekey? key2 (cdr subtable))))
			    (if record (cdr record) nil))
			  nil))))
	(insert-in-table-l (defun insert-temp (key1 key2 value)
			     (let ((subtable (assoc-l samekey? key1 (cdr table))))
			       (if subtable 
				   (let ((record (assoc-l samekey? key2 (cdr subtable))))
				     (if record (rplacd record value)
					 (rplacd subtable (cons (cons key2 value) (cdr subtable)))))
				   (rplacd table (cons (list key1 (cons key2 value)) (cdr table)))))))
	(dispatch (defun dispatch-temp (m)
		    (cond ((equalp m 'lookup) lookup-l)
			  ((equalp m 'insert) insert-in-table-l)))))
    dispatch))
(defun assoc-l (samekey? key records)
  (cond ((equalp records nil) nil)
	((funcall samekey? key (caar records)) (car records))
	(t (assoc-l samekey? key (cdr records)))))
			  