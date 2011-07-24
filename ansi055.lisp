(defun custom-precedes (x v y pos)
  (if (= pos (length v)) nil
       (if (eq (aref v pos) x)
	   (cons y (custom-precedes x v x (+ pos 1)))
	   (custom-precedes x v (aref v pos) (+ pos 1)))))
(defun remove-dups-sorted (v prev)
  (if (null v) nil
      (if (not (eq (car v) prev))
	  (cons (car v) (remove-dups-sorted (cdr v) (car v)))
	  (remove-dups-sorted (cdr v) prev))))
