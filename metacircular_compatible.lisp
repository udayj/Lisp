; this code by eli bendersky -  making modifications on top of this
(setf *print-circle* t)

; Used to debug the process of evaluation.
; 0 - no debugging printouts
; 1 - print out each evaluation and application
; 2 - also print out lookups of variables in eval.
; 3 - also print out the frame search process in
;     lookup-variable-value
;
(setf *evaluator-debug-level* 0)

;;;;;;;;;;;;;;;;
;
; Evaluator machinery
;
; eval. and apply. (followed by period to avoid clash
; with CL's built-ins) are the main driving engines,
; calling each other in a mutually recursive manner.
; 
(defun eval. (exp env)
  (when (> *evaluator-debug-level* 0)
    (format t "evaluating ~a~%" exp))
  (cond ((self-evaluating? exp) 
          exp)
        ((variable? exp) 
          (cond
            ((> *evaluator-debug-level* 1)
              (format t "~a is a variable~%" exp)
              (format t "looking up in env: ~a~%" env)
              (let ((res (lookup-variable-value exp env)))
                (format t "looked up: ~a~%" res)
                res))
            (t
              (lookup-variable-value exp env))))
        ((quoted? exp) 
          (text-of-quotation exp))
        ((assignment? exp)
          (eval-assignment exp env))
        ((definition? exp)
          (eval-definition exp env))
        ((if? exp)
          (eval-if exp env))
        ((or? exp)
          (eval-or exp env))
        ((and? exp)
          (eval-and exp env))
        ((let? exp)
          (eval. (let->combination exp) env))
        ((let*? exp)
          (eval. (let*->nested-lets exp) env))
        ((letrec? exp)
          (eval. (letrec->let exp) env))
        ((lambda? exp)
          (make-procedure 
            (lambda-parameteres exp)
            (lambda-body exp)
            env))
        ((begin? exp)
          (eval-sequence (begin-actions exp) env))
        ((cond? exp)
          (eval. (cond->if exp) env))
        ((while? exp)
          (eval. (while->combination exp) env))
        ((make-unbound? exp)
          (unbind-variable! exp env))
        ((application? exp)
          (when (> *evaluator-debug-level* 2)
            (format t "applying ~a~%" exp))
          (apply. 
            (eval. (operator exp) env)
            (list-of-values (operands exp) env)))
        (t
          (error "Unknown expression in EVAL: " exp))))
(defun actual-value (exp env)
  (force-it (eval. exp env)))
(defun list-of-arg-values (exp env)
  (if (no-operands? exp)
      nil
      (cons (actual-value (first-operand exp) env)
	    (list-of-arg-values (cdr exp) env))))
(defun list-of-delayed-args (exp env)
  (if (no-operands? exp)
      nil
      (cons (delay-it (first-operand exp) env)
	    (list-of-delayed-args (cdr exp) env))))
(defun delay-it (exp env)
  (list 'thunk exp env))
(defun delay-it-memo (exp env)
  (list 'thunk-memo exp env))
(defun thunk-memo? (obj)
  (tagged-list? obj 'thunk-memo))
(defun thunk? (obj)
  (tagged-list? obj 'thunk))
(defun thunk-exp (thunk)
  (cadr thunk))
(defun thunk-env (thunk)
  (caddr thunk))
(defun evaluated-thunk? (obj)
  (tagged-list? obj 'evaluated-thunk))
(defun thunk-value (evaluated-thunk)
  (cadr evaluated-thunk))
(defun force-it (obj)
  (cond ((thunk-memo? obj)
	 (let ((result (actual-value (thunk-exp obj)
				     (thunk-env obj))))
	   (rplaca obj 'evaluated-thunk )
	   (rplaca (cdr obj) result)
	   (rplacd (cdr obj) nil)
	   result))
	((thunk? obj)
	 (actual-value (thunk-exp obj)
		       (thunk-env obj)))
	((evaluated-thunk? obj)
	 (thunk-value obj))
	(t obj)))

(defun apply. (proc args)
  (when (> *evaluator-debug-level* 0)
    (format t "applying ~a to ~a~%" proc args))
  (cond ((primitive-procedure? proc)
          (apply-primitive-procedure proc (list-of-arg-values args)))
        ((compound-procedure? proc)
          (eval-sequence
            (procedure-body proc)
            (extend-environment
              (procedure-parameters proc)
              args
              (procedure-env proc))))
        (t
          (error
            "Unknown procedure type in APPLY: " proc))))


(defun list-of-values (exps env)
  (if (no-operands? exps)
      nil
      (let ((operand (first-operand exps)))
	(if (consp operand)
	    (if (eq (cadr operand) 'lazy)
		(cons (delay-it operand env)
		      (list-of-values (rest-operands exps) env))
		(cons (delay-it-memo operand env)
		      (list-of-values (rest-operands exps) env)))
	    (cons (eval. operand env)
		  (list-of-values (rest-operands exps) env))))))
	
(defun eval-if (exp env)
  (if (true? (eval. (if-predicate exp) env))
    (eval. (if-consequent exp) env)
    (eval. (if-alternative exp) env)))

(defun eval-sequence (exps env)
  (cond ((last-exp? exps) (eval. (first-exp exps) env))
        (t
          (eval. (first-exp exps) env)
          (eval-sequence (rest-exps exps) env))))

(defun eval-assignment (exp env)
  (set-variable-value! 
    (assignment-variable exp)
    (eval. (assignment-value exp) env)
    env)
  'ok)

(defun eval-definition (exp env)
  (define-variable!
    (definition-variable exp)
    (eval. (definition-value exp) env)
    env)
  'ok)

(defun self-evaluating? (exp)
  (or (numberp exp) (stringp exp)))

(defun variable? (exp)
  (symbolp exp))

(defun tagged-list? (exp tag)
  (if (consp exp)
    (eq (car exp) tag)
    nil))

; (quote <text-of-quotation>)
(defun make-quoted (exp) (list 'quote exp))
(defun quoted? (exp) (tagged-list? exp 'quote))
(defun text-of-quotation (exp) (cadr exp))

; (set! <var> <value>)
(defun make-assignment (var val) (list 'set! var val))
(defun assignment? (exp) (tagged-list? exp 'set!))
(defun assignment-variable (exp) (cadr exp))
(defun assignment-value (exp) (caddr exp))

; Two kinds of definitions:
;   (define <var> <value>)
;   or
;   (define (<var> <param1> ... <paramN>)
;     <body>)
;
(defun definition? (exp) (tagged-list? exp 'define))
(defun definition-variable (exp)
  (if (symbolp (cadr exp))
    (cadr exp)
    (caadr exp)))
(defun definition-value (exp)      
  (if (symbolp (cadr exp))
    (caddr exp)
    (make-lambda
      (cdadr exp)       ; formal parameters
      (cddr exp))))     ; body

; (lambda <params> <body statement> ...)
(defun make-lambda (params body)
  (cons 'lambda (cons params body)))
(defun lambda? (exp) (tagged-list? exp 'lambda))
(defun lambda-parameteres (exp) (cadr exp))
(defun lambda-body (exp) (cddr exp))

; (let ((<var1> <init1>) ... (<varN> <initN>))
;   <body>)
;
(defun let? (exp) (tagged-list? exp 'let))
(defun make-let (initforms body)
  (list 'let initforms body))
(defun let-body (exp) (caddr exp))
(defun let-initforms (exp) (cadr exp))
(defun let-vars (exp)
  (mapcar #'car (let-initforms exp)))
(defun let-inits (exp)
  (mapcar #'cadr (let-initforms exp)))
(defun named-let? (exp)
  (atom (cadr exp)))
(defun named-let-name (exp) (cadr exp))
(defun named-let-vars (exp) (let-vars (cdr exp)))
(defun named-let-inits (exp) (let-inits (cdr exp)))
(defun named-let-body (exp) (let-body (cdr exp)))

(defun let->combination (exp)
  (if (named-let? exp)
    (sequence->exp
      (list
        (list 
          'define 
          (cons (named-let-name exp) (named-let-vars exp))
          (named-let-body exp))
        (cons
          (cadr exp)
          (named-let-inits exp))))
    (cons
      (make-lambda (let-vars exp) (list (let-body exp)))
      (let-inits exp))))

; let* is similar to let, except that the bindings 
; of the let variables are performed sequentially from 
; left to right, and each binding is made in an 
; environment in which all of the preceding bindings 
; are visible
;
(defun let*? (exp) (tagged-list? exp 'let*))

(defun let*->nested-lets (exp)
  (labels (
      (make-rec-let (initforms body)
        (if (null initforms)
          body
          (make-let
            (list (car initforms))
            (make-rec-let (cdr initforms) body)))))
    (make-rec-let (cadr exp) (caddr exp))))

; letrec expressions, which have the form
;
; (letrec (<var1> <exp1>) ... (<varN> <expN>))
;    <body>)
;
; are a variation on -let- in which the expressions
; <expK> that provide the initial values for the 
; variables <varK> are evaluated in an environment 
; that includes all the letrec bindings. This permits 
; recursion in the bindings.
;
(defun letrec? (exp) (tagged-list? exp 'letrec))

(defun letrec->let (exp)
  "Transforms into a let, such that all variables
  are created with a let and then assigned their
  values with set!"
  ; Note that since letrec is identical in syntax to
  ; let, we can freely use the let- accessors.
  ;
  (let ((initforms (let-initforms exp))
        (body (let-body exp)))
    (make-let
      (mapcar ; initforms
        #'(lambda (initform)
            (list 
              (car initform) 
              (make-quoted '*unassigned*)))
        initforms)
      (make-begin ; body
        (append
          (mapcar
            #'(lambda (initform)
                (make-assignment
                  (car initform)
                  (cadr initform)))
              initforms)
            (list body))))))

; (if <predicate> <consequent> <alternative>)
(defun make-if (predicate consequent alternative)
  (list 'if predicate consequent alternative))
(defun if? (exp) (tagged-list? exp 'if))
(defun if-predicate (exp) (cadr exp))
(defun if-consequent (exp) (caddr exp))
(defun if-alternative (exp) 
  (if (not (null (cdddr exp)))
    (cadddr exp)
    'false))

; (while <condition> <body>)
(defun while? (exp) (tagged-list? exp 'while))
(defun while-condition (exp) (cadr exp))
(defun while-body (exp) (caddr exp))

(defun while->combination (exp)
  (sequence->exp
    (list
      (list 
        'define 
        (list 'while-iter)
        (sequence->exp
          (list
            (while-body exp)            
            (make-if 
              (while-condition exp)
              (list 'while-iter)
              'true))))
      (list 'while-iter))))

; (or <exp1> ... <expN>)
(defun or? (exp) (tagged-list? exp 'or))

(defun eval-or (exp env)
  (dolist (e (cdr exp) nil)
    (let ((result (eval. e env)))
      (when (true? result)
        (return result)))))

; (and <exp1> ... <expN>)
(defun and? (exp) (tagged-list? exp 'and))

(defun eval-and (exp env)
  (dolist (e (cdr exp) (car (last exp)))
    (let ((result (eval. e env)))
      (when (false? result)
        (return nil)))))

; some convenience procedures for the evaluator
(defun begin? (exp) (tagged-list? exp 'begin))
(defun begin-actions (exp) (cdr exp))
(defun make-begin (seq) (cons 'begin seq))
(defun last-exp? (seq) (null (cdr seq)))
(defun first-exp (seq) (car seq))
(defun rest-exps (seq) (cdr seq))

; transform a sequence into a single expression, using
; 'begin' if necessary
;
(defun sequence->exp (seq)
  (cond ((null seq) seq)
        ((last-exp? seq) (first-exp seq))
        (t (make-begin seq))))

; A procedure application is any compount expression that
; is not one of the above expression types. The _car_ of
; the expression is the operator, and the _cdr_ is the list
; of operands
;
(defun application? (exp) (consp exp))
(defun operator (exp) (car exp))
(defun operands (exp) (cdr exp))
(defun no-operands? (ops) (null ops))
(defun first-operand (ops) (car ops))
(defun rest-operands (ops) (cdr ops))

; (cond (clause1) ... (clauseN))
; each clause is: <predicate> <actions>
; a special predicate 'else is for the 
; default case.
;
(defun cond? (exp) (tagged-list? exp 'cond))
(defun cond-clauses (exp) (cdr exp))
(defun cond-else-clause? (clause) 
  (eq (cond-predicate clause) 'else))
(defun cond-predicate (clause) (car clause))
(defun cond-actions (clause) (cdr clause))
(defun cond->if (exp) 
  (expand-cond-clauses (cond-clauses exp)))

(defun expand-cond-clauses (clauses)
  (if (null clauses)
    'false              ; no _else_ clause
    (let ((first (car clauses))
          (rest (cdr clauses)))
      (if (cond-else-clause? first)
        (if (null rest)
          (sequence->exp (cond-actions first))
          (error "ELSE clause isn't last " clauses))
        (make-if
          (cond-predicate first)
          (sequence->exp (cond-actions first))
          (expand-cond-clauses rest))))))

; Testing of predicates
;
(defun true? (x)
  (not (false? x)))

(defun false? (x)
  (null x))

; Procedures
(defun scan-out-defines (body)
  "Takes a procedure body and returns an equivalent 
  one that has no internal definition, by 
  transforming:
  
  (lambda <vars>
    (define u <e1>)
    (define v <e2>)
    <e3>)
  
  Into:
  
  (lambda <vars>
    (let ((u '*unassigned*)
          (v '*unassigned*))
      (set! u <e1>)
      (set! v <e2>)
      <e3>))"
  (let ((defines '())
        (non-defines '()))
    (dolist (exp body)
      (if (definition? exp)
        (push exp defines)
        (push exp non-defines)))
    (if (null defines)
      body
      (progn
        ; The order of non-defines is important, so
        ; we restore the order that was reversed by
        ; using -push-. The order of defines, OTOH,
        ; is not important
        ;
        (nreverse non-defines)
        (list 
          (make-let
            (mapcar 
              #'(lambda (def)
                  (list 
                    (definition-variable def) 
                    (make-quoted '*unassigned*)))
                defines)
            (make-begin
              (append
                (mapcar
                  #'(lambda (def)
                      (make-assignment
                        (definition-variable def)
                        (definition-value def)))
                    defines)
                non-defines))))))))

(defun make-procedure (parameters body env) 
  (list 
    'procedure 
    parameters 
    ; body
    (scan-out-defines body)
    env))

(defun compound-procedure? (p)
  (tagged-list? p 'procedure))
(defun procedure-parameters (p)
  (parameter-list (cadr p)))
(defun parameter-list (exps)
  (if (null exps) nil
      (if (consp (car exps))
	  (cons (caar exps)
		(parameter-list (cdr exps)))
	  (cons (car exps)
		(parameter-list (cdr exps))))))
(defun procedure-body (p) (caddr p))
(defun procedure-env (p) (cadddr p))

;;;;;;;;;;;;;;;;
;
; Environments
;
; An environment is a sequence of frames, where 
; each frame is a table of bindings that associate
; variables with their corresponding values.
;

; An environment is a list of frames. 
; The enclosing environment is the _cdr_ of the 
; list. The empty environment is the empty list.
;
(defun enclosing-environment (env) (cdr env))
(defun first-frame (env) (car env))
(setf the-empty-environment '())

; Each frame of an environment is represented
; as a pair of lists: a list of the variables
; bound in that frame and a list of the
; associated values.
;
(defun make-frame (vars vals)
  (if (/= (length vars) (length vals))
    (error "MAKE-FRAME length mismatch")
    (cons vars vals)))

(defun frame-variables (frame) (car frame))
(defun frame-values (frame) (cdr frame))
(defun add-binding-to-frame! (frame var val)
  (setf (car frame) (cons var (car frame)))
  (setf (cdr frame) (cons val (cdr frame))))

(defun find-binding-in-frame (frame var)
  "Looks up the variable in the frame.
  Returns a pair: if the -car- of the pair is t,
  then the variable was found and it's in the -cdr-
  of the pair. If the -car- of the pair is nil,
  then the variable was not found"
  (labels (
      (scan (vars vals)
        (cond ((null vars)
                (cons nil nil))
              ((eq var (car vars))
                (cons t (car vals)))
              (t 
                (scan (cdr vars) (cdr vals))))))
    (scan (frame-variables frame)
          (frame-values frame))))

(defun set-binding-in-frame! (frame var val)
  "Sets the variable to the value in the frame.
  Returns t if the variable was found and modified,
  nil otherwise."
  (labels (
      (scan (vars vals)
        (cond ((null vars)
                nil)
              ((eq var (car vars))
                (setf (car vals) val)
                t)
              (t
                (scan (cdr vars) (cdr vals))))))
    (scan (frame-variables frame)
          (frame-values frame))))

(defun unbind-var-in-frame! (frame var)
  "Unbinds a variable in the frame."
  (let ((vars (frame-variables frame))
        (vals (frame-values frame))
        (new-vars '())
        (new-vals '()))
    (loop
      for a-var in vars
      for a-val in vals
      do 
      (unless (eq a-var var)
        (push a-var new-vars)
        (push a-val new-vals)))
    (setf (car frame) new-vars)
    (setf (cdr frame) new-vals)))

; Debugging: Prints the variables defined in
; a frame.
; Note: definitions of compound procedures contain
; a reference to the environment in which they're
; defined. This is a circular reference, and 
; printing such variables will crash the interpreter
; unless *print-circle* is defined.
;
(defun print-frame (frame &optional (sep "::> "))
  (let ((vars (frame-variables frame))
        (vals (frame-values frame)))
    (loop
      for var in vars
      for val in vals
      do 
        (format t "~a~a = ~a~%" sep var val))))

; To extend an environment by a new frame that 
; associates variables with values, we make a frame 
; consisting of the list of variables and the list 
; of values, and we adjoin this to the environment. 
; We signal an error if the number of variables 
; does not match the number of values
;
(defun extend-environment (vars vals base-env)
  (if (= (length vars) (length vals))
    (cons (make-frame vars vals) base-env)
    (if (< (length vars) (length vals))
      (error "Too many arguments " vars vals)
      (error "Too few arguments " vars vals))))

(defun lookup-variable-value (var env)
  (labels ( 
      (env-loop (env)
        (when (> *evaluator-debug-level* 2)
          (format t "scanning env: ~a~%" env))
        (if (eq env the-empty-environment)
          (error "Unbound variable ~a" var))
          (let* ( (result (find-binding-in-frame (first-frame env) var))
                  (found (car result))
                  (value (cdr result)))
            (if found
              (if (eq value '*unassigned*)
                (error "Using an unassigned var ~a" var)
                value)
              (env-loop (enclosing-environment env))))))
    (env-loop env)))

(defun set-variable-value! (var val env)
  (labels (
      (env-loop (env)
        (if (eq env the-empty-environment)
          (error "Unbound variable ~a" var)
          (if (set-binding-in-frame! (first-frame env) var val)
            t
            (env-loop (enclosing-environment env))))))
    (env-loop env)))

(defun define-variable! (var val env)
  (let ((frame (first-frame env)))
    (if (set-binding-in-frame! frame var val)
      t
      (add-binding-to-frame! frame var val))))

(defun make-unbound? (exp) 
  (tagged-list? exp 'make-unbound!))

(defun unbind-variable! (exp env)
  (unbind-var-in-frame! (first-frame env) (cadr exp)))

;;;;;;;;;;;;;;;;
;
; Setting up the global environment
;
; It does not matter how we represent the primitive 
; procedure objects, so long as apply. can identify 
; and apply them by using the procedures 
; primitive-procedure? and apply-primitive-procedure. 
; We have chosen to represent a primitive procedure 
; as a list beginning with the symbol primitive 
; and containing a procedure in the underlying 
; Lisp that implements that primitive.
;
(defun primitive-procedure? (proc)
  (tagged-list? proc 'primitive))
(defun primitive-implementation (proc) (cadr proc))

; Note that the names of the primitive procedures are
; Scheme, and the implementation Common Lisp (i.e.
; 'null?' is implemented by 'null'. This is because 
; the language we implement here is Scheme, while the
; language the evaluator is written in is CL.
;
(setf primitive-procedures
  (list 
    (list 'car #'car)
    (list 'cdr #'cdr)
    (list 'cadr #'cadr)
    (list 'cddr #'cddr)
    (list 'cons #'cons)
    (list 'null? #'null)
    (list 'assoc #'assoc)
    (list 'list #'list)
    (list '= #'=)
    (list '> #'>)
    (list '< #'<)
    (list 'eq? #'eq)
    (list '+ #'+)
    (list '- #'-)
    (list '* #'*)
    (list '/ #'/)))

(setf primitive-procedure-names
  (mapcar #'car primitive-procedures))

(setf primitive-procedure-objects
  (mapcar 
    (lambda (proc)
      (list 'primitive (cadr proc)))
    primitive-procedures))

; Note CL's own 'apply' is used here, because 
; the primitive procedures belong to the 
; underlying, and not the interpreted language.
;
(defun apply-primitive-procedure (proc args)
  (apply (primitive-implementation proc) args))

(defun setup-env ()
  (let ((initial-env
          (extend-environment
            primitive-procedure-names
            primitive-procedure-objects
            the-empty-environment)))
    (define-variable! 'true t initial-env)
    (define-variable! 'false nil initial-env)
    initial-env))

(setf the-global-environment (setup-env))

(defun interpret (exp)
  (eval. exp the-global-environment))

(defun eval-opt. (exp env)
  (funcall (analyze. exp) env))
(defun analyze. (exp)
  (cond ((self-evaluating? exp)
	 (analyze-self-evaluating exp))
	((quoted? exp)
	 (analyze-quoted exp))
	((variable? exp)
	 (analyze-variable exp))
	((assignment? exp)
	 (analyze-assignment exp))
	((definition? exp)
	 (analyze-definition exp))
	((if-exp? exp)
	 (analyze-if exp))
	((lambda? exp)
	 (analyze-lambda exp))
	((begin? exp)
	 (analyze-sequence (begin-actions exp)))
	((cond? exp)
	 (analyze. (cond->if exp)))
	((application? exp)
	 (analyze-application exp))
	(t
	 (error "Unknown expression in EVAL:" exp))))
(defun analyze-self-evaluating (exp)
  (lambda (env) exp))
(defun analyze-quoted-exp (exp)
  (let((qval (text-of-quotation exp)))
    (lambda (env) qval)))
(defun analyze-variable (exp)
  (lambda (env)
    (lookup-variable-value exp env)))
(defun analyze-assignment (exp)
  (let ((var (assignment-variable exp))
	(vproc (analyze. (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (funcall vproc env) env)
      'ok)))
(defun analyze-definition (exp)
  (let ((var (definition-variable exp))
	(vproc (analyze. (definition-value exp))))
    (lambda (env)
      (define-variable! var (funcall vproc env) env)
      'ok)))
(defun analyze-if (exp)
  (let ((pproc (analyze. (if-predicate exp)))
	(cproc (analyze. (if-predicate exp)))
	(aproc (analyze. (if-predicate exp))))
    (lambda (env)
      (if (true? (funcall pproc env))
	  (funcall cproc env)
	  (funcall aproc env)))))
(defun analyze-lambda (exp)
  (let ((vars (lambda-parameters exp))
	(bproc (analyze. (lambda-body exp))))
    (lambda (env)
      (make-procedure vars bproc env))))
(defun analyze-sequence (exp)
  (labels (
	   (sequentially (proc1 proc2)
	     (lambda (env)
	       (funcall proc1 env)
	       (funcall proc2 env)))
	   (sloop (first-proc rest-procs)
	     (if (null rest-procs)
		 first-proc
		 (sloop
		  (sequentially firsrt-proc (car rest-procs))
		  (cdr rest-procs)))))
    (let ((procs (mapcar #'analyze. exps)))
      (if (null procs)
	  (error "EMPTY sequence in analyze sequence")
	  (sloop (car procs) (cdr procs))))))
(defun analyze-application (exp)
  (let ((fproc (analyze. (operator exp)))
	(aprocs (mapcar #'analyze. (operands exp))))
    (lambda (env)
      (execute-application 
       (funcall fproc env)
       (mapcar 
	(lambda (aproc)
	  (funcall aproc env))
	aprocs)))))
(defun execute-application (proc args)
  (cond ((primitive-procedure? proc)
	 (apply-primitive-procedure proc args))
	((compound-procedure? proc)
	 (funcall
	  (procedure-body proc)
	  (extend-environment 
	   (procedure-parameters proc)
	   args
	   (procedure-env proc))))
	(t
	 (error "Unknown procedure type -- EXECUTE application ~a" proc))))
(defun interpret-opt (exp)
  (eval-opt. exp  the-global-environment))
	