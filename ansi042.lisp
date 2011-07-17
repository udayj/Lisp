(defun custom-reverse (l)
  (reduce (lambda (x y) (cons y x)) l :initial-value nil))
(defun custom-copy-list (l)
  (reduce (lambda (x y) (append x (list y))) l :initial-value nil))