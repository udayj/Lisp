(defun intersperse (obj l)
  (if (null (cdr l)) l
      (append (list (car l) obj) (intersperse obj (cdr l)))))