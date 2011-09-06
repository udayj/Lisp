;code below is by Eli Bendersky (with some custom modifications)
(defvar *debug-logic* 0)
(defvar *op-table* (make-hash-table :test #'equal))
(defvar the-empty-stream nil)
(defun put-op (op type proc)
  (setf (gethash (list op type) *op-table*) proc))
(defun get-op (op type)
  (gethash (list op type) *op-table*))
;Frames and binding
(defun make-binding (var val)
  (cons var val))
(defun binding-variable (binding)
  (car binding))
(defun binding-value (binding)
  (cdr binding))
(defun binding-in-frame (var frame)
  (assoc var frame :test #'equal))
(defun extend (var val frame)
  (cons (make-binding var val) frame))
;Database of assertions
(defvar *the-assertions* the-empty-stream)
(defun fetch-assertions (pattern frame)
  (if (use-index? pattern)
      (get-indexed-assertions pattern)
      (get-all-assertions)))
(defun get-all-assertions()
  *the-assertions*)
(defun get-indexed-assertions (pattern)
  (get-stream (index-key-of pattern) 'assertion-stream))
(defun get-stream (key1 key2)
  (let ((s (get-op key1 key2)))
    (if s
	s
	the-empty-stream)))
(defvar *the-rules* the-empty-stream)
(defun fetch-rules (pattern frame)
  (if (use-index? pattern)
      (get-indexed-rules pattern)
      (get-all-rules)))
(defun get-all-rules () *the-rules*)
(defun get-indexed-rules (pattern)
  (stream-append
   (get-stream (index-key-of pattern) 'rule-stream)
   (get-stream '? 'rule-stream)))
(defun add-rule-or-assertion! (assertion)
  (if (rule? assertion)
      (add-rule! assertion)
      (add-assertion! assertion)))
(defun add-assertion! (assertion)
  (store-assertion-in-index assertion)
  (let ((old-assertions *the-assertions*))
    (setf *the-assertions*
	  (cons-stream assertion old-assertions))
    'ok))
(defun add-rule! (rule)
  (store-rule-in-index rule)
  (let ((old-rules *the-rules*))
    (setf *the-rules*
	  (cons-stream rule old-rules))
    'ok))

(defun store-assertion-in-index (assertion)
  (if (indexable? assertion)
      (let ((key (index-key-of assertion)))
	(let ((current-assertion-stream
	       (get-stream key 'assertion-stream)))
	  (put-op
	   key
	   'assertion-stream
	   (cons-stream assertion current-assertion-stream))))))
(defun store-rule-in-index (rule)
  (let ((pattern (conclusion rule)))
    (if (indexable? pattern)
	(let ((key (index-key-of pattern)))
	  (let ((current-rule-stream
		 (get-stream key 'rule-stream)))
	    (put-op
	     key
	     'rule-stream
	     (cons-stream rule current-rule-stream)))))))
(defun indexable? (pat)
  (or (constant-symbol? (car pat))
      (var? (car pat))))
(defun index-key-of (pat)
  (let ((key (car pat)))
    (if (var? key) '? key)))
(defun use-index? (pat)
  (constant-symbol? (car pat)))
;Some new stream operations
(defun stream-append-delayed (s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream
       (stream-car s1)
       (stream-append-delayed (stream-cdr s1) delayed-s2))))
(defun interleave (s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream
       (stream-car s1)
       (interleave s2 (stream-cdr s1)))))
(defun interleave-delayed (s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream
       (stream-car s1)
       (interleave-delayed
	(force delayed-s2)
	(delay (stream-cdr s1))))))
(defun stream-flatmap (proc s)
  (flatten-stream (stream-map proc s)))
(defun flatten-stream (stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave-delayed
       (stream-car stream)
       (delay (flatten-stream (stream-cdr stream))))))
; Alternative implementation without delaying
;
; (defun flatten-stream (stream)
  ; (if (stream-null? stream)
    ; the-empty-stream
    ; (interleave
      ; (stream-car stream)
      ; (flatten-stream (stream-cdr stream)))))
(defun simple-stream-flatmap (proc s)
  (simple-flatten (stream-map proc s)))
(defun simple-flatten (stream)
  (stream-map
   #'stream-car
   (stream-filter
    (lambda (s)
      (not (stream-null? s)))
    stream)))
(defun singleton-stream (x)
  "Create a stream consisting of a single element"
  (cons-stream x the-empty-stream))
;Query Evaluator
(defun tagged-list? (exp tag)
  (if (consp exp)
      (eq (car exp) tag)
      nil))
(defun type-exp (exp)
  (if (consp exp)
      (car exp)
      (error "Unknown expression TYPE-EXP ~a" exp)))
(defun contents (exp)
  (if (consp exp)
      (cdr exp)
      (error "Unknown expression CONTENTS ~a" exp)))
(defun assertion-to-be-added? (exp)
  (eq (type-exp exp) 'assert!))
(defun add-assertion-body (exp)
  (car (contents exp)))
(defun empty-conjunction? (exps) (null exps))
(defun first-conjunct (exps)
  (car exps))
(defun rest-conjuncts (exps)
  (cdr exps))
(defun empty-disjunction? (exps)
  (null exps))
(defun first-disjunct (exps)
  (car exps))
(defun rest-disjuncts (exps)
  (cdr exps))
(defun negated-query (exps)
  (car exps))
(defun predicate (exps)
  (car exps))
(defun args (exps)
  (cdr exps))
(defun rule? (statement)
  (tagged-list? statement 'rule))
(defun conclusion (rule)
  (cadr rule))
(defun rule-body (rule)
  (if (null (cddr rule))
      '(always-true)
      (caddr rule)))
(defun query-syntax-process (exp)
  (map-over-symbols #'expand-question-mark exp))
(defun map-over-symbols (proc exp)
  (cond ((consp exp)
	 (cons (map-over-symbols proc (car exp))
	       (map-over-symbols proc (cdr exp))))
	((symbolp exp)
	 (funcall proc exp))
	(t exp)))
(defun expand-question-mark (symb)
  (let ((chars (string symb)))
    (if (equal (char chars 0) #\?)
	(list '? (intern (remove #\? chars :count 1)))
	symb)))
(defun var? (exp)
  (tagged-list? exp '?))
(defun constant-symbol? (exp)
  (symbolp exp))
(defvar *rule-counter* 0)
(defun new-rule-application-id ()
  (incf *rule-counter*)
  *rule-counter*)
(defun make-new-variable (var rule-app-id)
  (cons '? (cons rule-app-id (cdr var))))
(defun contract-question-mark (var)
  (intern 
   (concatenate 'string
		"?"
		(if (numberp (cadr var))
		    (concatenate 'string
				 (string (caddr var))
				 "-"
				 (write-to-string (cadr var)))
		    (string (cadr var))))))
(defun qeval (query frame-stream)
  "The qeval procedure is the basic 
  evaluator of the query system. It 
  takes as inputs a query and a stream 
  of frames, and it returns a stream 
  of extended frames. It identifies 
  special forms by a data-directed 
  dispatch using get. Any query that 
  is not identified as a special form 
  is assumed to be a simple query, 
  to be processed by simple-query."
(let ((qproc (get-op (type-exp query) 'qeval)))
  (if qproc
      (funcall qproc (contents query) frame-stream)
      (simple-query query frame-stream))))
(defun simple-query (query-pattern frame-stream)
  (when (>= *debug-logic* 3)
    (format t "- query:~a~%" query-pattern)
    (format t "-- frames: ~a~%" (mapcar (lambda (x)
					(cons '* x))
					(stream->list frame-stream))))
  (stream-flatmap
   (lambda (frame)
     (stream-append-delayed
      (find-assertions query-pattern frame)
      (delay (apply-rules query-pattern frame))))
   frame-stream))
; Alternative implementation without delaying
;
; (defun simple-query (query-pattern frame-stream)
  ; "Takes as arguments a simple query (a pattern) 
  ; together with a stream of frames, and it 
  ; returns the stream formed by extending each 
  ; frame by all data-base matches of the query"
  
  ; (when (>= *debug-logic* 3)
    ; (format t "- query: ~a~%" query-pattern)
    ; (format t "-- frames: " )  
    ; (display-stream frame-stream))
  
  ; (stream-flatmap
    ; (lambda (frame)
      ; (stream-append
        ; (find-assertions query-pattern frame)
        ; (apply-rules query-pattern frame)))
    ; frame-stream))
(defun disjoin (disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave-delayed
       (qeval (first-disjunct disjuncts) frame-stream)
       (delay
	(disjoin (rest-disjuncts disjuncts) frame-stream)))))
; Alternative implementation without delaying
;
; (defun disjoin (disjuncts frame-stream)
  ; "The output streams for the various disjuncts 
  ; of the -or- are computed separately and merged"
  ; (if (empty-disjunction? disjuncts)
    ; the-empty-stream
    ; (interleave
      ; (qeval (first-disjunct disjuncts) frame-stream)
      ; (disjoin (rest-disjuncts disjuncts) frame-stream))))
(put-op 'or 'qeval #'disjoin)
(defun conjoin (conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (conjoin
       (rest-conjuncts conjuncts)
       (qeval (first-conjunct conjuncts) frame-stream))))
(defun merge-frame-streams (s1 s2)
  (stream-flatmap
   (lambda (f1)
     (stream-filter
      (lambda (f)
	(not (equal f 'failed)))
      (stream-map 
       (lambda (f2)
	 (merge-frames f1 f2))
       s2)))
   s1))
(put-op 'and 'qeval #'conjoin)
(defun negate (operands frame-stream)
  (simple-stream-flatmap
   (lambda (frame)
     (if (stream-null?
	  (qeval
	   (negated-query operands)
	   (singleton-stream frame)))
	 (singleton-stream frame)
	 the-empty-stream))
   frame-stream))
(put-op 'not 'qeval #'negate)
(defun singleton-stream? (stream)
  (and
   (stream-car stream)
   (not (stream-cadr stream))))
(defun query-of-unique (uniq)
  (car uniq))
(defun uniquely-asserted (query frame-stream)
  "Return only those frames for whom the query
   produces a single match in the database"
  (simple-stream-flatmap
    (lambda (frame)
      (let ((eval-stream 
              (qeval 
                (query-of-unique query) 
                (singleton-stream frame))))
        (if (singleton-stream? eval-stream)
          eval-stream
          the-empty-stream)))
    frame-stream))
(put-op 'unique 'qeval #'uniquely-asserted)
(defun lisp-value (call frame-stream)
  (simple-stream-flatmap
   (lambda (frame)
     (if (execute-exp
	  (instantiate
	   call
	   frame
	   (lambda (v f)
	     (error "Unknown pat var -- LISP-VALUE ~a~%" v))))
	 (singleton-stream frame)
	 the-empty-stream))
   frame-stream))
(defun execute-exp (exp)
  (apply (eval (predicate exp)) (args exp)))
(put-op 'lisp-value 'qeval #'lisp-value)
(defun always-true (ignore frame-stream) frame-stream)
(put-op 'always-true 'qeval #'always-true)
(defun instantiate (exp frame unbound-var-handler)
  (labels (
	   (copy (exp)
	     (cond ((var? exp)
		    (let ((binding (binding-in-frame exp frame)))
		      (if binding
			  (copy (binding-value binding))
			  (funcall unbound-var-handler exp frame))))
		   ((consp exp)
		    (cons (copy (car exp)) (copy (cdr exp))))
		   (t exp))))
    (copy exp)))
(defun find-assertions (pattern frame)
  (simple-stream-flatmap
   (lambda (datum)
     (check-an-assertion datum pattern frame))
   (fetch-assertions pattern frame)))
(defun check-an-assertion (assertion query-pat query-frame)
  (let ((match-result
	 (pattern-match query-pat assertion query-frame)))
    (if (eq match-result 'failed)
	the-empty-stream
	(singleton-stream match-result))))
(defun pattern-match (pat dat frame)
  (cond ((eq frame 'failed) 'failed)
	((equal pat dat) frame)
	((var? pat) (extend-if-consistent pat dat frame))
	((and (consp pat) (consp dat))
	 (pattern-match
	  (cdr pat)
	  (cdr dat)
	  (pattern-match 
	   (car pat)
	   (car dat)
	   frame)))
	(t 'failed)))
(defun extend-if-consistent (var dat frame)
  (let ((binding (binding-in-frame var frame)))
    (if binding
	(pattern-match (binding-value binding) dat frame)
	(extend var dat frame))))
(defun apply-rules (pattern frame)
  (stream-flatmap
   (lambda (rule)
     (apply-a-rule rule pattern frame))
   (fetch-rules pattern frame)))
(defun apply-a-rule (rule query-pattern query-frame)
  (let ((clean-rule (rename-variables-in rule)))
    (let ((unify-result
	   (unify-match
	    query-pattern
	    (conclusion clean-rule)
	    query-frame)))
      (if (eq unify-result 'failed)
	  the-empty-stream
	  (qeval
	   (rule-body clean-rule)
	   (singleton-stream unify-result))))))
(defun rename-variables-in (rule)
  (let ((rule-application-id (new-rule-application-id)))
    (labels (
	     (tree-walk (exp)
	       (cond ((var? exp)
		      (make-new-variable exp rule-application-id))
		     ((consp exp)
		      (cons (tree-walk (car exp))
			    (tree-walk (cdr exp))))
		     (t exp))))
      (tree-walk rule))))
(defun unify-match (p1 p2 frame)
  (cond ((eq frame 'failed) 'failed)
	((equal p1 p2) frame)
	((var? p1) (extend-if-possible p1 p2 frame))
	((var? p2) (extend-if-possible p2 p1 frame))
	((and (consp p1) (consp p2))
	 (unify-match
	  (cdr p1)
	  (cdr p2)
	  (unify-match
	   (car p1)
	   (car p2)
	   frame)))
	(t 'failed)))
(defun extend-if-possible (var val frame)
  (let ((binding (binding-in-frame var frame)))
    (cond (binding
	   (unify-match
	    (binding-value binding)
	    val frame))
	  ((var? val)
	   (let ((binding (binding-in-frame val frame)))
	     (if binding
		 (unify-match var (binding-value binding) frame)
		 (extend var val frame))))
	  ((depends-on? val var frame) 'failed)
	  (t (extend var val frame)))))
(defun depends-on? (exp var frame)
  (labels (
	   (tree-walk (e)
	     (cond ((var? e)
		    (if (equal var e)
			t
			(let ((b (binding-in-frame e frame)))
			  (if b
			      (tree-walk (binding-value b))
			      nil))))
		   ((consp e)
		    (or (tree-walk (car e))
			(tree-walk (cdr e))))
		   (t nil))))
    (tree-walk exp)))
(defun merge-frames (f1 f2)
  (if (null f1)
      f2
      (let ((var (caar f1))
	    (val (cdar f1)))
	(let ((extension (extend-if-possible var val f2)))
	  (if (equal extension 'failed)
	      'failed
	      (merge-frames (cdr f1) extension))))))
(defun qinterpret (&rest exps)
  (dolist (exp exps)
    (let ((q (query-syntax-process exp)))
      (cond ((assertion-to-be-added? q)
	     (add-rule-or-assertion! (add-assertion-body q)))
	    (t
	     (display-stream
	      (stream-map
	       (lambda (frame)
		 (instantiate
		  q
		  frame
		  (lambda (v f)
		    (contract-question-mark v))))
	       (qeval q (singleton-stream '()))) 5))))))
(defun stream->list (s)
  (if (stream-null? s)
      nil
      (cons (stream-car s) (stream->list (stream-cdr s)))))
;;Stream operations
	   
(defun memo-proc (proc)
  (let ((already-run? nil)
	(result nil))
    (lambda ()
      (if (not already-run?)
	  (progn
	    (setf result (funcall proc))
	    (setf already-run? t)
	    result)
	  result))))
(defmacro delay (expr)
  `(memo-proc (lambda () ,expr)))
(defmacro cons-stream (a b)
  `(cons ,a (delay ,b)))
(defun force (delayed-object)
  (funcall delayed-object))
(defun stream-car (s)
  (car s))
(defun stream-cdr (s)
  (force (cdr s)))
(defun stream-cadr (s)
	(stream-car (stream-cdr s)))
(defun enumerate-interval (low high)
  (if (> low high)
      nil
      (cons-stream low (enumerate-interval (+ low 1) high))))
(defun show (x)
  (format t "~a~%" x)
  x)
(defun stream-map (proc s)
  (if (equalp nil s) nil
      (cons-stream (funcall proc (stream-car s)) (stream-map proc (stream-cdr s)))))
(defun stream-ref (s n)
  (if (= 0 n) (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))
(defun stream-filter (pred stream)
  (cond ((equalp nil stream) nil)
	((funcall pred (stream-car stream))
	 (cons-stream (stream-car stream) (stream-filter pred (stream-cdr stream))))
	(t (stream-filter pred (stream-cdr stream)))))

(defun integers-starting-from (n)
  (cons-stream n (integers-starting-from (+ n 1))))
(defun sieve (stream)
  (cons-stream (stream-car stream)
	       (sieve (stream-filter (lambda (x) (not (= 0 (mod x (stream-car stream))))) (stream-cdr stream)))))
(defun stream-map1 (proc &rest argstreams)
  (if (equalp nil (car argstreams))
    nil
    (cons-stream
      (apply proc (mapcar #'stream-car argstreams))
      (apply #'stream-map1
        (cons proc (mapcar #'stream-cdr argstreams))))))
(defun add-stream (s1 s2)
  (stream-map1 #'+ s1 s2))
(defun mul-stream (s1 s2)
  (stream-map1 #'* s1 s2))

(defun partial-sums1 (s)
  (cons-stream (stream-car s) (add-stream (stream-cdr s) (partial-sums1 s))))
(defun scale-stream (stream factor)
  (stream-map1 (lambda (x) (* x factor)) stream))
(defun merge1 (s1 s2)
  (cond ((equalp nil s1) s2)
	((equalp nil s2) s1)
	(t
	 (let ((s1car (stream-car s1))
	       (s2car (stream-car s2)))
	   (cond ((< s1car s2car) (cons-stream s1car (merge1 (stream-cdr s1) s2)))
		 ((> s1car s2car) (cons-stream s2car (merge1 s1 (stream-cdr s2))))
		 (t (cons-stream s1car (merge1 (stream-cdr s1) (stream-cdr s2)))))))))
(defun square (x)
  (* x x))


(defun pairs1 (s t1)
  (cons-stream (list (stream-car s) (stream-car t1))
	       (interleave (stream-map1 (lambda (x) (list (stream-car s) x)) (stream-cdr t1)) 
			   (pairs1 (stream-cdr s) (stream-cdr t1)))))
(defun display-stream (stream n)
  (if (or (= 0 n) (stream-null? stream)) nil
      (progn 
      (show (stream-car stream))
      (display-stream (stream-cdr stream) (- n 1)))))

(defun stream-null? (s)
  (eq s the-empty-stream))
(defun stream-append (s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1) (stream-append (stream-cdr s1) s2))))