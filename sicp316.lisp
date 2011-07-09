(defun count-pairs (x)
  (if (not (consp x)) 0
      (+ (count-pairs (car x))
	 (count-pairs (cdr x))
	 1)))
(defun count-pairs-correct(x pair-set)
  (let (
	(func (defun func1 (x)
		(if (or (not (consp x)) (contains? pair-set x)) 0
		    (progn
		      (setf pair-set (add-to-set pair-set x))
		      (+ (func1 (car x))
			 (func1 (cdr x))
			 1))))))
    (funcall func x)))
(defun add-to-set (set x)
  (cons x set))
(defun contains? (set x)
  (cond 
    ((nil? set) nil)
    ((eq (car set) x) t)
    (t (contains? (cdr set) x))))

(defun nil? (x)
  (equalp nil x))