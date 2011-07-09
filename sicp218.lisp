(defun nil? (x)
  (equalp nil x))
(defun reverse-list(x)
  (if (nil? x) nil
      (append (reverse-list (rest x)) (list (first x)))))
(defun last-pair(x)
  (list (first (reverse-list x))))