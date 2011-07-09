(defun get-first-elements (seq)
  (if (nil? seq) nil
      (cons (car (car seq)) (get-first-elements (cdr seq)))))
(defun get-rem-elements (seq)
  (if (nil? seq) nil
      (append (list (cdr (car seq))) (get-rem-elements (cdr seq)))))
(defun accumulate-n (op init seqs)
  (cond ((nil? (car seqs)) nil)
	(t (cons (accumulate op init (get-first-elements seqs))
		 (accumulate-n op init (get-rem-elements seqs))))))