(defun make-table ()
  (list 'table))
(defun lookup (key table)
  (let ((record (assoc1 key (cdr table))))
    (if record (cdr record) nil)))
(defun assoc1 (key records)
  (cond ((equalp nil records) nil)
	((equalp key (caar records)) (car records))
	(t (assoc1 key (cdr records)))))
(defun insert-in-table (key value table)
  (let ((record (assoc1 key (cdr table))))
    (if record (rplacd record value)
	(rplacd table (cons (cons key value) (cdr table))))))
(defun lookup-2d (key1 key2 table)
  (let ((subtable (assoc1 key1 (cdr table))))
    (if subtable
	(let ((record (assoc1 key2 (cdr subtable))))
	  (if record (cdr record) nil))
	nil)))
(defun insert-in-table-2d (key1 key2 value table)
  (let ((subtable (assoc1 key1 (cdr table))))
    (if subtable 
	(let ((record (assoc1 key2 (cdr subtable))))
	  (if record
	      (rplacd record value)
	      (rplacd subtable (cons (cons key2 value) (cdr subtable)))))
	(rplacd table (cons (list key1 (cons key2 value)) (cdr table))))))