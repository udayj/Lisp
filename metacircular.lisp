(defun eval. (exp env)
  (cond
    ((self-evaluating? exp) exp)
    ((variable? exp) (lookup-variable-value exp env))
    ((quoted? exp) (text-of-quotation exp))
    ((assigbment? exp) (eval-assignment exp env))
    ((definition? exp) (eval-definition exp env))
    ((if? exp) (eval-if exp env))
    ((lambda? exp)
     (make-procedure (lambda-parameters exp)
		     (lambda-body exp)
		     env))
    ((begin? exp)
     (eval-sequence (begin-actions exp) env))
    ((cond? exp) (eval. (cond->if exp) env))
    ((and? exp) (eval-and exp env))
    ((or? exp) (eval-or exp env))
    ((let? exp) (eval. (let->combination exp) env))
    ((application? exp)
     (apply (eval. (operator exp) env)
	    (list-of-values (operands exp) env)))
    (t (error "unknown expression in EVAL: " exp))))
(defun apply. (procedure arguments)
  (cond ((primitive-procedure? procedure)
	 (apply-primitive-procedure procedure arguments))
	((compound-procedure? procedure)
	 (eval-sequence
	  (procedure-body procedure)
	  (extend-environment (procedure-parameters procedure)
			      arguments
			      (procedure-environment procedure))))
	(t (error "Unknown procedure type -- APPLY " procedure))))
(defun list-of-values (exps env)
  (if (no-operands? exps)
      nil
      (cons (eval. (first-operand exps) env)
	    (list-of-values (rest-operands exps) env))))
(defun eval-if (exp env)
  (if (true? (eval. (if-predicate exp)
		   env))
      (eval. (if-consequent exp) env)
      (eval. (if-alternative exp) env)))
(defun eval-sequence (exps env)
  (cond ((last-exp? exps) (eval. (first-exp exps) env))
	(t
	 (eval. (first-exp exps) env)
	 (eval-sequence (rest-exps exps) env))))
(defun eval-assignment (exp env)
  (set-variable-value! (assignment-variable exp)
		       (eval. (assignment-value exp) env)
		       env)
  'ok)
(defun eval-definition (exp env)
  (define-variable! (definition-variable exp)
      (eval. (definition-value exp) env)
    env)
  'ok)
(defun self-evaluating? (exp)
  (cond ((numberp exp) t)
	((stringp exp) t)
	(t nil)))
(defun variable? (exp)
  (symbolp exp))
(defun quoted? (exp)
  (tagged-list? exp 'quote))
(defun text-of-quotation (exp)
  (cadr exp))
(defun tagged-list? (exp tag)
  (if (consp exp)
      (eq? (car exp) tag)
      nil))
(defun assignment? (exp)
  (tagged-list? exp 'set!))
(defun assignment-variable (exp)
  (cadr exp))
(defun assignment-value (exp)
  (caddr exp))
(defun definition? (exp)
  (tagged-list? exp 'define))
(defun definition-variable (exp)
  (if (symbolp (cadr exp))
      (cadr exp)
      (caadr exp)))
(defun definition-value (exp)
  (if (symbolp (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
		   (cddr exp))))
(defun lambda? (exp)
  (tagged-list? exp 'lambda))
(defun lambda-parameters (exp)
  (cadr exp))
(defun lambda-body (exp)
  (cddr exp))
(defun make-lambda (parameters body)
  (cons 'lambda (cons parameters body)))
(defun if? (exp)
  (tagged-list? exp 'if))
(defun if-predicate (exp)
  (cadr exp))
(defun if-consequent (exp)
  (caddr exp))
(defun if-alternative (exp)
  (if (not (null (cdddr exp)))
      (cadddr exp)
      'false))
(defun make-if (predicate consequent alternative)
  (list 'if predicate consequent alternative))
(defun begin? (exp)
  (tagged-list? exp 'begin))
(defun begin-actions (ex)
  (cdr exp))
(defun last-exp? (seq)
  (null (cdr seq)))
(defun first-exp (seq)
  (car seq))
(defun rest-exps (seq)
  (cdr seq))
(defun sequence->exp (seq)
  (cond ((null seq) seq)
	((last-exp? seq) (first-exp seq))
	(t (make-begin seq))))
(defun make-begin (seq)
  (cons 'begin seq))
(defun application? (exp)
  (consp exp))
(defun operator (exp)
  (car exp))
(defun operands (exp)
  (cdr exp))
(defun no-operands? (ops)
  (null ops))
(defun first-operands (ops)
  (car ops))
(defun rest-operands (ops)
  (cdr ops))
(defun cond? (exp)
  (tagged-list? exp 'cond))
(defun cond-clauses (exp)
  (cdr exp))
(defun cond-else-clause? (clause)
  (eq (cond-predicate? clause) 'else))
(defun cond-predicate (clause)
  (car clause))
(defun cond-actions (clause)
  (cdr clause))
(defun cond->if (exp)
  (expand-clauses (cond-clauses exp)))
(defun expand-clauses (clauses)
  (if (null clauses)
      'false
      (let ((first (car clauses))
	    (rest (cdr clauses)))
	(if (cond-else-clause? first)
	    (if (null rest)
		(sequence->exp (cond-actions first))
		(error "ELSE clause isn't last -- COND->IF " clauses))
	    (make-if (cond-predicate first)
		     (sequence->exp (cond-actions first))
		     (expand-clauses rest))))))
(defun and? (exp)
  (tagged-list? exp 'and))
(defun eval-and (exp env)
  (if (null exp) 'true
      (let ((first (first-exp exp))
	    (rest (rest-exp exp)))
	(if (true? (eval. first env))
	    (eval-and rest env)
	    'false))))
(defun or? (exp)
  (tagged-list? exp 'or))
(defun eval-or (exp env)
  (if (null exp) 'false
      (let ((first (first-exp exp))
	    (rest (rest-exp exp)))
	(if (true? (eval. first env))
	    'true
	    (eval-or rest env)))))
(defun let? (exp) (tagged-list? exp 'let))
(defun let-body (exp) (cddr exp))
(defun let-vars (exp)
  (mapcar #'car (cadr exp)))
(defun let-inits (exp)
  (mapcar #'cadr (cadr exp)))

(defun let->combination (exp)
  (cons
    (make-lambda (let-vars exp) (list (let-body exp)))
    (let-inits exp)))
(defun let*? (exp)
  (tagged-list? exp 'let*))
(defun let*->nested-lets (exp)
  (labels
      (
       (make-let-rec (init body)
	 (if (null init) body
	     (make-let (car init)
		       (make-let-rec (cdr init) body)))))
    (make-let-rec (cadr exp)(caddr exp))))
(defun make-let (init boyd)
  (list 'let init body))
(defun true? (x)
  (not (eq x 'false)))
(defun false? (x)
  (eq x 'false))

(defun make-procedure (parameters body env)
  (list 'procedure parameters body env))
(defun compound-procedure? (p)
  (tagged-list? p 'procedure))
(defun procedure-parameters (p)
  (cadr p))
(defun procedure-body (p)
  (caddr p))
(defun procedure-environment (p)
  (cadddr p))
(defun enclosing-environment (env)
  (cdr env))
(defun first-frame (env)
  (car env))
(deflex the-empty-environment '())
(defun make-frame (variables values)
  (cons variables values))
(defun frame-variables (frame)
  (car frame))
(defun frame-values (frame)
  (cdr frame))
(defun add-binding-to-frame! (var val frame)
  (rplaca frame (cons var (car frame)))
  (rplacd frame (cons val (cdr frame))))
(defun extend-environment (vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (error "Number of vars / vals dont match -- EXTEND-ENV" (cons vars vals))))
(defun lookup-variable-value (var env)
  (labels ((env-loop (env)
	     (labels ((scan (vars vals)
			(cond ((null vars)
			       (env-loop (enclosing-environment env)))
			      ((eq? var (car vars))
			      (car vals))
			      (t
			       (scan (cdr vars) (cdr vals))))))
	       (if (eq env the-empty-environment)
		   (error "Unbounded variable var" var)
		   (let ((frame (first-frame env)))
		     (scan (frame-variables first)
			   (frame-values first)))))))
    (env-loop env)))
(defun set-variable-value! (var val env)
  (labels ((env-loop (env)
	     (labels ((scan (vars vals)
			(cond ((null vars)
			       (env-loop (enclosing-environment env)))
			      ((eq val (var vars))
			       (rplaca vals val))
			      (t (scan (cdr vars) (cdr vals))))))
	       (if (eq env the-empty-environment)
		   (error "Unbound variable" var)
		   (let ((frame (first-frame env)))
		     (scan (frame-variables frame)
			   (frame-values frame)))))))
    (env-loop env)))
(defun define-variable! (var val env)
  (let ((frame (first-frame env)))
    (labels ((scan (vars vals)
	       (cond ((null vars)
		      (add-binding-to-frame! var val frame))
		     ((eq var (car vars))
		      (rplaca vals val))
		     (t (scan (cdr vars) (cdr vals))))))
      (scan (frame-variables frame)
	    (frame-values frame)))))
	      



		    
			    
			