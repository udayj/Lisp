(defstruct (node (:print-function
		  (lambda (n s d)
		    (format s "#<~A>" (node-elt n)))))
  elt
  (c1 nil)
  (c2 nil)
  (c3 nil))
(defun copy-node-tree (n)
  (if (null n) nil
      (make-node
       :elt (node-elt n)
       :c1 (copy-tree (node-c1 n))
       :c2 (copy-tree (node-c2 n))
       :c3 (copy-tree (node-c3 n)))))
(defun find-node (obj n)
  (if (null n) nil
      (or
       (eql obj (node-elt n))
       (find-node obj (node-c1 n))
       (find-node obj (node-c2 n))
       (find-node obj (node-c3 n)))))