(defun new-withdraw ()
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
	  (progn
	    (setf balance (- balance amount))
	    balance)
	  'error))))
(defun make-withdraw (balance)
  (lambda (amount)
    (if (>= balance amount)
	(progn 
	  (setf balance (- balance amount))
	  balance)
	'error)))
(defun make-account (balance actual-password)
  (let ((withdraw-int 
    (defun withdraw (amount)
      (if (>= balance amount)
	  (progn 
	    (setf balance (- balance amount))
	    balance))))
    (deposit-int (defun deposit (amount)
      (progn
	(setf balance (+  balance amount))
	balance)))
	(counter 0)
	(error-function (defun error-function () 'incorrect-password-entered-5-times-consecutively)))
    (lambda (m input-password)
      (if (equalp input-password actual-password)
	  (progn
	    (setf counter 0)
      (cond ((equalp m 'withdraw) withdraw-int)
	    ((equalp m 'deposit) deposit-int)))
      (progn
	(incf counter)
	(if (>= counter 5) (funcall error-function) 'wrong-password))))))

(defun make-monitored1 (procedure)
  (let ((counter 0))
  (lambda (arg)
    (cond ((equalp arg 'how-many-calls?) counter)
	  ((equalp arg 'reset-counter) (progn 
					 (setf counter 0)
					 counter))
	  (t (progn
	       (incf counter)
	       (funcall procedure arg)))))))
(defun square1 (x)
  (* x x))
(defun make-joint (account orig-password new-password)
  (lambda ( method input-password)
    (if (equalp input-password new-password)
	(funcall account method orig-password)
	'wrong-password)))
  