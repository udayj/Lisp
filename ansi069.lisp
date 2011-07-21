(defun apply-like (fn args)
  (let ((*print-base* 8))
    (apply fn args)))