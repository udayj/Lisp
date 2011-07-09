(defun decode(input-string offset)
  (setf alpha (string "abcdefghijklmnopqrstuvwxyz"))
  (setf table (make-hash-table))
  (do ((i 0 (1+ i))
       (pointer (- 0 offset) pointer))
      ((= i 26) nil)
    (setf (gethash (aref alpha i) table)
	  (aref alpha (mod (+ i (mod pointer 26)) 26))))
  (loop
        :for ch :across input-string
        :collect (gethash ch table) into str-out
	:finally (format t "~a~%" (coerce str-out 'string)))
  (map 'string #'(lambda (x)
		   (gethash x table)) input-string))
  
  

       