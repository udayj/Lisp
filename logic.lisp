(load "sicp-stream")

(defvar *debug-logic* 0)

;;----------------------------------------------------
;; Dispatch table.
;;
;; Operation, type -> procedure
;; 
(defvar *op-table* (make-hash-table :test #'equal))

(defun put-op (op type proc)
  (setf (gethash (list op type) *op-table*) proc))

(defun get-op (op type)
  (gethash (list op type) *op-table*))

;;----------------------------------------------------
;; Frames and bindings
;;
(defun make-binding (var val) (cons var val))
(defun binding-variable (binding) (car binding))
(defun binding-value (binding) (cdr binding))
(defun binding-in-frame (var frame) (assoc var frame :test #'equal))
(defun extend (var val frame) (cons (make-binding var val) frame))

;;----------------------------------------------------
;; Database of assertions and rules
;;

(defvar *the-assertions* the-empty-stream)

(defun fetch-assertions (pattern frame)
  (if (use-index? pattern)
    (get-indexed-assertions pattern)
    (get-all-assertions)))

(defun get-all-assertions () *the-assertions*)
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

;;----------------------------------------------------
;; Some new stream operations
;;

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

;;----------------------------------------------------
;; Query evaluator
;;
;; In general, it uses the following method to apply
;; a rule when tryung to establish a query pattern in
;; a frame that specifies bindings for some of the 
;; pattern variables:
;;
;;   * Unify the query with the conclusion of the 
;;     rule to form, if successful, an extension of
;;     the original frame.
;;   * Relative to the extended frame, evaluate the 
;;     query formed by the body of the rule.
;;
;; Given the query pattern and a stream of frames, we
;; produce, for each frame in the input stream, two
;; streams:
;;   
;;   * A stream of extended frames obtained by 
;;     matching the pattern against all assertiosn in
;;     the data base (using the pattern matcher)
;;   * A stream of extended frames obtained by 
;;     applying all possible rules (using the unifier)
;;
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
(defun first-conjunct (exps) (car exps))
(defun rest-conjuncts (exps) (cdr exps))
(defun empty-disjunction? (exps) (null exps))
(defun first-disjunct (exps) (car exps))
(defun rest-disjuncts (exps) (cdr exps))
(defun negated-query (exps) (car exps))
(defun predicate (exps) (car exps))
(defun args (exps) (cdr exps))

(defun rule? (statement) (tagged-list? statement 'rule))
(defun conclusion (rule) (cadr rule))
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
        ((symbolp exp) (funcall proc exp))
        (t exp)))

(defun expand-question-mark (symb)
  (let ((chars (string symb)))
    (if (equal (char chars 0) #\?)
      (list '? (intern (remove #\? chars :count 1)))
      symb)))

(defun var? (exp)
  (tagged-list? exp '?))
(defun constant-symbol? (exp) (symbolp exp))

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
  "Takes as arguments a simple query (a pattern) 
  together with a stream of frames, and
  returns the stream formed by extending each 
  frame by all data-base matches of the query"
  
  (when (>= *debug-logic* 3)
    (format t "- query: ~a~%" query-pattern)
    (format t "-- frames: ~a~%" (mapcar #'(lambda (x) (cons '* x)) (stream->list frame-stream))))
  
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
  "The output streams for the various disjuncts 
  of the -or- are computed separately and merged"
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
  "_and_ queries are handled by the conjoin procedure. 
  Conjoin takes as inputs the conjuncts and the frame 
  stream and returns the stream of extended frames. 
  First, conjoin processes the stream of frames to 
  find the stream of all possible frame extensions 
  that satisfy the first query in the conjunction. 
  Then, using this as the new frame stream, it 
  recursively applies conjoin to the rest of the 
  queries"
  (if (empty-conjunction? conjuncts) 
    frame-stream      
    (conjoin 
      (rest-conjuncts conjuncts)
      (qeval (first-conjunct conjuncts) frame-stream))))

(defun merge-frame-streams (s1 s2)
  "Tries to merge each frame of s1 with each frame
  of s2. Returns the stream of successful merges."
  (stream-flatmap
    (lambda (f1)
      (stream-filter
        (lambda (f) (not (equal f 'failed)))
        (stream-map
          (lambda (f2)
            (merge-frames f1 f2))
          s2)))
    s1))

(put-op 'and 'qeval #'conjoin)

(defun negate (operands frame-stream)
  "We attempt to extend each frame in the input 
  stream to satisfy the query being negated, 
  and we include a given frame in the output 
  stream only if it cannot be extended."
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

(defun query-of-unique (uniq) (car uniq))

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
  "Each frame in the stream is used to instantiate 
  the variables in the pattern, the indicated 
  predicate is applied, and the frames for which 
  the predicate returns false are filtered out 
  of the input stream. An error results if there 
  are unbound pattern variables."
  (simple-stream-flatmap
    (lambda (frame)
      (if (execute-exp
            (instantiate
              call
              frame
              (lambda (v f)
                (error "Unknown pat var -- LISP-VALUE ~a" v))))
        (singleton-stream frame)
        the-empty-stream))
    frame-stream))

(defun execute-exp (exp)
  (apply (eval (predicate exp)) (args exp)))

(put-op 'lisp-value 'qeval #'lisp-value)

(defun always-true (ignore frame-stream) frame-stream)
(put-op 'always-true 'qeval #'always-true)

(defun instantiate (exp frame unbound-var-handler)
  "To instantiate an expression, we copy it, 
  replacing any variables in the expression by 
  their values in a given frame. The values are 
  themselves instantiated, since they could 
  contain variables (for example, if ?x in exp 
  is bound to ?y as the result of unification 
  and ?y is in turn bound to 5). The action to 
  take if a variable cannot be instantiated is 
  given by a procedural argument to instantiate."
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
  "Takes as input a pattern and a frame. 
  Returns a stream of frames, each extending 
  the given one by a data-base match of the 
  given pattern. It uses fetch-assertions to 
  get a stream of all the assertions in the 
  data base that should be checked for a 
  match against the pattern and the frame. 
  The reason for fetch-assertions here is 
  that we can often apply simple tests that 
  will eliminate many of the entries in the 
  data base from the pool of candidates for a 
  successful match. The system would still 
  work if we eliminated fetch-assertions and 
  simply checked a stream of all assertions 
  in the data base, but the computation would 
  be less efficient because we would need to 
  make many more calls to the matcher."
  (simple-stream-flatmap 
    (lambda (datum)
      (check-an-assertion datum pattern frame))
    (fetch-assertions pattern frame)))

(defun check-an-assertion (assertion query-pat query-frame)
  "Takes as arguments a pattern, a data object (assertion), 
  and a frame and returns either a one-element stream 
  containing the extended frame or the-empty-stream if the 
  match fails"
  (when (>= *debug-logic* 4)
    (format t "check-an-assertion: ~a~% ~a (frame: ~a)~%" query-pat assertion query-frame))
  (let ((match-result
        (pattern-match query-pat assertion query-frame)))
    (when (>= *debug-logic* 4)
      (format t "  result: ~a~%" match-result))
    (if (eq match-result 'failed)
      the-empty-stream
      (singleton-stream match-result))))

(defun pattern-match (pat dat frame)
  "The basic pattern matcher returns either the symbol 
  'failed or an extension of the given frame. The basic 
  idea of the matcher is to check the pattern against the 
  data, element by element, accumulating bindings for the 
  pattern variables. If the pattern and the data object 
  are the same, the match succeeds and we return the 
  frame of bindings accumulated so far. Otherwise, if the 
  pattern is a variable we extend the current frame by 
  binding the variable to the data, so long as this is 
  consistent with the bindings already in the frame. If 
  the pattern and the data are both pairs, we 
  (recursively) match the car of the pattern against the 
  car of the data to produce a frame; in this frame we 
  then match the cdr of the pattern against the cdr 
  of the data. If none of these cases are applicable, 
  the match fails and we return the symbol 'failed."
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
  "If there is no binding for the variable in the 
  frame, we simply add the binding of the variable 
  to the data. Otherwise we match, in the frame, the 
  data against the value of the variable in the frame. 
  If the stored value contains only constants, as 
  it must if it was stored during pattern matching 
  by extend-if-consistent, then the match simply 
  tests whether the stored and new values are the same. 
  If so, it returns the unmodified frame; if not, 
  it returns a failure indication. The stored value 
  may, however, contain pattern variables if it 
  was stored during unification. The recursive 
  match of the stored pattern against the new data 
  will add or check bindings for the variables 
  in this pattern.
  For example, suppose we have a frame in which ?x 
  is bound to (f ?y) and ?y is unbound, and we wish 
  to augment this frame by a binding of ?x to (f b). 
  We look up ?x and find that it is bound to (f ?y). 
  This leads us to match (f ?y) against the proposed 
  new value (f b) in the same frame. Eventually this 
  match extends the frame by adding a binding of ?y 
  to b. ?X remains bound to (f ?y). 
  We never modify a stored binding and we never store 
  more than one binding for a given variable."
  (let ((binding (binding-in-frame var frame)))
    (if binding
      (pattern-match (binding-value binding) dat frame)
      (extend var dat frame))))

(defun apply-rules (pattern frame)
  "Takes as input a pattern and a frame, 
  and forms a stream of extension frames 
  by applying rules from the data base."
  (stream-flatmap
    (lambda (rule)
      (apply-a-rule rule pattern frame))
    (fetch-rules pattern frame)))

(defun apply-a-rule (rule query-pattern query-frame)
  "Augments its argument frame by unifying the rule 
  conclusion with the pattern in the given frame. 
  If this succeeds, evaluates the rule body in 
  this new frame.
  Before any of this happens, however, the program 
  renames all the variables in the rule with unique 
  new names. The reason for this is to prevent 
  the variables for different rule applications 
  from becoming confused with each other. "
  (when (>= *debug-logic* 4)
    (format t "apply-a-rule: ~a~%  q-pattern: ~a ~%"
      rule query-pattern)
    (format t "  frame: " )
    (display-stream query-frame)
    (format t "~%"))
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
  "We generate unique variable names by associating 
  a unique identifier (such as a number) with each 
  rule application and combining this identifier 
  with the original variable names."
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
  "Takes as inputs two patterns and a frame and 
  returns either the extended frame or the symbol 
  failed. The unifier is like the pattern matcher 
  except that it is symmetrical -- variables are 
  allowed on both sides of the match. Unify-match 
  is basically the same as pattern-match, except 
  that there is extra code (marked ** below) to 
  handle the case where the object on the right 
  side of the match is a variable."
  (cond ((eq frame 'failed) 'failed)
        ((equal p1 p2) frame)
        ((var? p1) (extend-if-possible p1 p2 frame))
        ((var? p2) (extend-if-possible p2 p1 frame))    ; **
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
  "The same as the extend-if-consistent used in pattern 
  matching except for two special checks, marked ** below. 
  In the first case, if the variable we are trying to 
  match is not bound, but the value we are trying to 
  match it with is itself a (different) variable, it is 
  necessary to check to see if the value is bound, and 
  if so, to match its value. If both parties to the match 
  are unbound, we may bind either to the other.
  The second check deals with attempts to bind a variable 
  to a pattern that includes that variable. Such a 
  situation can occur whenever a variable is repeated in 
  both patterns."
  (let ((binding (binding-in-frame var frame)))
    (cond (binding 
            (unify-match 
              (binding-value binding) val frame))
          ((var? val)                                   ; **
            (let ((binding (binding-in-frame val frame)))
              (if binding
                (unify-match var (binding-value binding) frame)
                (extend var val frame))))
          ((depends-on? val var frame) 'failed)         ; **
          (t (extend var val frame)))))

(defun depends-on? (exp var frame)
  "Tests whether an expression depends on 
  the variable. This must be done relative to 
  the current frame because the expression may 
  contain occurrences of a variable that already 
  has a value that depends on our test variable."
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
                (or (tree-walk (car e)) (tree-walk (cdr e))))
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

;;----------------------------------------------------
;; Non-interactive driver 
;;
(defun qinterpret (&rest exps)
  (dolist (exp exps)
    (setf *deduction-history* '())
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
                  (qeval q (singleton-stream '())))))))))

;;----------------------------------------------------
;; An even less interactive driver 
;; Returns the stream instead of displaying it.
;; Works for a single expression only.
;;
(defun qinterpret2 (exp)
  (let ((q (query-syntax-process exp)))
    (cond ((assertion-to-be-added? q)
            (add-rule-or-assertion! (add-assertion-body q)))
          (t
            (stream->list
              (stream-map
                (lambda (frame)
                  (instantiate
                    q
                    frame
                    (lambda (v f)
                      (contract-question-mark v))))
                (qeval q (singleton-stream '()))))))))
                
;;----------------------------------------------------
;; Like qinterpret, but limits the amount of printed
;; items
;;
(defun qinterpret3 (&rest exps)
  (dolist (exp exps)
    (let ((q (query-syntax-process exp)))
      (cond ((assertion-to-be-added? q)
              (add-rule-or-assertion! (add-assertion-body q)))
            (t
              (display-n-stream-elements 
                (stream-map
                  (lambda (frame)
                    (instantiate
                      q
                      frame
                      (lambda (v x)
                        (contract-question-mark v) x)))
                  (qeval q (singleton-stream '())))5 ))))))

(defun stream->list (s)
  (if (stream-null? s)
    '()
    (cons (stream-car s) (stream->list (stream-cdr s)))))

(defun display-n-stream-elements(x)
x
)