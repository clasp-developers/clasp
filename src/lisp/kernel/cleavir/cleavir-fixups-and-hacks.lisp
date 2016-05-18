;;; This file contains hacks and fixups for Cleavir that beach plans to fix by changing Cleavir
;;;  For now I'll redefine Cleavir functions and classes in this file and load it after
;;;  Cleavir is loaded
;;;  Search for #+cleavir-hack to find the hacks

(eval-when (:execute :compile-toplevel)
  (push-new :cleavir-hack *features*))

#+asdf(defmethod asdf:perform :after ((op asdf:load-op) (sys (eql (asdf:find-system :cleavir))))
                 (load #.(or *compile-file-pathname* *load-pathname*)))
;;; ------------------------------------------------------------
;;;
;;; lambda-list &va-rest hack
;;;
;;; See issue #249 - this hack adds support for the core:&va-rest lambda list keyword
;;;

;;; ---------- Changes to Code/Cleavir/Code-utilities/lambda-lists.lisp

(in-package #:cleavir-code-utilities)

(defparameter *lambda-list-keywords*
  `((&whole 1 1)
    (&environment 1 nil)
    (&optional 0 nil)
    (&rest 1 1)
    (&body 1 1)
    #+cleavir-hack(core:&va-rest 1 1)
    (&key 0 nil)
    (&allow-other-keys 0 0)
    (&aux 0 nil)))

(defun check-lambda-list-keywords (lambda-list keywords)
  ;; We assume that KEYWORDS is a subset of LAMBDA-LIST-KEYWORDS, in
  ;; other words that we are given only valid lambda list keywords as
  ;; defined by the system.
  (let* (;; All symbols in the lambda list that look like they might
	 ;; be lambda-list keywords, in the order that the occur in
	 ;; the lambda list.  Multiple occurrences are preserved.
	 (potential (loop for remaining = lambda-list then (cdr remaining)
			  while (consp remaining)
			  when (potential-lambda-list-keyword-p (car remaining))
			    collect (car remaining)))

	 ;; All symbols in the lambda list that are also lambda-list
	 ;; keywords as defined by the system, in the order that they
	 ;; occur in the lambda list. 
	 (real (remove-if-not (lambda (x) (member x lambda-list-keywords))
			      potential))
	 ;; All symbols in the lambda list that look like they might
	 ;; be lambda-list keywords, but that are not lambda list
	 ;; keywords defined by the system, in any old order. 
	 (suspect (set-difference potential lambda-list-keywords))
	 ;; All symbols in the lambda list that are also lambda-list
	 ;; keywords as defined by the system, but that are not in the
	 ;; list of lambda list keywords allowed for this type of
	 ;; lambda list, in any old order.
	 (forbidden (set-difference real keywords))
	 ;; All symbols in the lambda list that are also in the list
	 ;; of valid keywords for this lambda list, in the order that
	 ;; they appear in the lambda list.  Multiple occurrences are
	 ;; preserved.
	 (to-process (remove-if-not (lambda (x) (member x keywords))
				    potential)))
    ;; Check for forbidden keywords.
    (unless (null forbidden)
	(error 'lambda-list-keyword-not-allowed
	       :form lambda-list
	       :keyword (car forbidden)))
    ;; Check for suspect keywords.
    (unless (null suspect)
      (warn 'suspect-lambda-list-keyword
	    :form lambda-list
	    :keyword (car suspect)))
    ;; Check for multiple occurrences.
    (loop for keyword in to-process
	  do (when (> (count keyword to-process) 1)
	       (error 'multiple-occurrences-of-lambda-list-keyword
		      :form lambda-list
		      :keyword keyword)))
    (when (> (+ (count '&body to-process) (count '&rest to-process) #+cleavir-hack(count 'core:&va-rest to-process)) 1)
      (error 'both-rest-and-body-occur-in-lambda-list
	     :form lambda-list))
    ;; Check the order of keywords.
    (loop for rem = to-process then (cdr rem)
	  until (null (cdr rem))
	  do (when (and (not (eq (car rem) '&environment))
			(not (eq (cadr rem) '&environment))
			(> (position (car rem) *lambda-list-keywords* :key #'car)
			   (position (cadr rem) *lambda-list-keywords* :key #'car)))
	       (error 'incorrect-keyword-order
		      :form lambda-list
		      :keyword1 (car rem)
		      :keyword2 (cadr rem))))
    ;; Check arities.
    (flet ((check-arity (keyword number-of-args)
	     (if (eq keyword '&whole)
		 (when (zerop number-of-args)
		   (error 'whole-must-be-followed-by-variable
			  :code lambda-list))
		 (let ((arities (cdr (assoc keyword *lambda-list-keywords*))))
		   (when (or (< number-of-args (car arities))
			     (and (not (null (cadr arities)))
				  (> number-of-args (cadr arities))))
		     (error "wrong arity for ~s" keyword))))))
      (loop with positions = (mapcar (lambda (x) (position x lambda-list))
				     to-process)
	    for keyword in to-process
	    for (pos next-pos) on (append positions
					  (list (list-structure lambda-list)))
	    do (check-arity keyword (- next-pos pos 1))))
    ;; Check that if &whole is present, it appears first.
    (when (and (member '&whole to-process)
	       (not (eq (car lambda-list) '&whole)))
      (error 'whole-must-appear-first
	     :code lambda-list))))

(defclass lambda-list ()
  (;; A possibly empty list of patterns.
   (%required :initform '() :initarg :required :accessor required)
   ;; Either:
   ;;  * :none, meaning &environment was not given, or
   ;;  * a single variable, represented as a symbol.
   (%environment :initform :none :initarg :environment :accessor environment)
   ;; Either:
   ;;  * :none, meaning &whole was not given, or
   ;;  * a single variable, represented as a symbol.
   (%whole :initform :none :initarg :whole :accessor whole)
   ;; Either:
   ;;  * :none, meaning &optional was not given at all,
   ;;  * a possibly empty list of &optional entries.
   (%optionals :initform :none :initarg :optionals :accessor optionals)
   ;; Either:
   ;;  * :none, meaning &rest or &body was not given at all, or
   ;;  * a single pattern.
   (%rest-body :initform :none :initarg :rest-body :accessor rest-body)
   ;; Either:
   ;;  * '&rest, meaning &rest or '&body or 'core:&va-rest
   #+cleavir-hack(%rest-name :initform '&rest :initarg :rest-name :accessor rest-name)
   ;; Either:
   ;;  * :none, meaning &key was not given at all,
   ;;  * a possibly empty list of &key entries.
   (%keys :initform :none :initarg :keys :accessor keys)
   ;; Either:
   ;;  * nil, meaning &allow-other-keys was not given at all,
   ;;  * t, meaning &allow-other-keys was given.
   (%allow-other-keys :initform nil
		      :initarg :allow-other-keys
		      :accessor allow-other-keys)
   ;; Either:
   ;;  * :none, meaning &aux was not given at all,
   ;;  * a possibly empty list of &aux entries.
   (%aux :initform '() :initarg :aux :accessor aux)))


(defun parse-rest/body (lambda-list positions)
  (cond ((and
	  ;; there is a keyword yet to be processed.
	  (not (null (cdr positions)))
	  ;; that keyword is &environment.
	  (or (eq (elt lambda-list (car positions)) '&rest)
	      (eq (elt lambda-list (car positions)) '&body)
	      #+cleavir-hack(eq (elt lambda-list (car positions)) 'core:&va-rest)
              ))
	 ;; The arity has already been checked so we know there is
	 ;; something after it, but we don't know what.
	 (let ((arg (elt lambda-list (1+ (car positions)))))
	   (unless (and (symbolp arg)
			(not (constantp arg)))
	     (error 'rest/body-must-be-followed-by-variable
		    :code lambda-list))
	   (values arg (cdr positions) #+cleavir-hack(elt lambda-list (car positions)))))
	(t
	 (values :none positions '&rest))))


(defun parse-ordinary-lambda-list (lambda-list)
  (let ((allowed '(&optional &rest #+cleavir-hack core:&va-rest &key &allow-other-keys &aux)))
    (check-lambda-list-proper lambda-list 'parse-ordinary-lambda-list)
    (check-lambda-list-keywords lambda-list allowed)
    (let ((positions (compute-keyword-positions lambda-list allowed))
	  (result (make-instance 'lambda-list)))
      (setf (required result)
	    (parse-all-required
	     lambda-list 0 (car positions) #'parse-ordinary-required))
      (setf (values (optionals result) positions)
	    (parse-all-optionals
	     lambda-list positions #'parse-ordinary-optional))
      (setf (values (rest-body result) positions #+cleavir-hack(rest-name result))
	    (parse-rest/body lambda-list positions))
      (setf (values (keys result) positions)
	    (parse-all-keys
	     lambda-list positions #'parse-ordinary-key))
      (setf (values (allow-other-keys result) positions)
	    (parse-allow-other-keys lambda-list positions))
      (setf (values (aux result) positions)
	    (parse-all-aux lambda-list positions))
      ;; We should have run out of parameters now.
      (unless (null (cdr positions))
	(error "something is seriously wrong here"))
      result)))


(defun parse-generic-function-lambda-list (lambda-list)
  (let ((allowed '(&optional &rest #+cleavir-hack core:&va-rest &key &allow-other-keys)))
    (check-lambda-list-proper lambda-list 'parse-generic-function-lambda-list)
    (check-lambda-list-keywords lambda-list allowed)
    (let ((positions (compute-keyword-positions lambda-list allowed))
	  (result (make-instance 'lambda-list)))
      (setf (required result)
	    (parse-all-required
	     lambda-list 0 (car positions) #'parse-ordinary-required))
      (setf (values (optionals result) positions)
	    (parse-all-optionals
	     lambda-list positions #'parse-defgeneric-optional))
      (setf (values (rest-body result) positions #+cleavir-hack (rest-name result))
	    (parse-rest/body lambda-list positions))
      (setf (values (keys result) positions)
	    (parse-all-keys
	     lambda-list positions #'parse-defgeneric-key))
      (setf (values (allow-other-keys result) positions)
	    (parse-allow-other-keys lambda-list positions))
      ;; We should have run out of parameters now.
      (unless (null (cdr positions))
	(error "something is seriously wrong here"))
      result)))
	
(defun parse-specialized-lambda-list (lambda-list)
  (let ((allowed '(&optional &rest #+cleavir-hack core:&va-rest &key &allow-other-keys &aux)))
    (check-lambda-list-proper lambda-list 'parse-specialized-lambda-list)
    (check-lambda-list-keywords lambda-list allowed)
    (let ((positions (compute-keyword-positions lambda-list allowed))
	  (result (make-instance 'lambda-list)))
      (setf (required result)
	    (parse-all-required
	     lambda-list 0 (car positions) #'parse-specialized-required))
      (setf (values (optionals result) positions)
	    (parse-all-optionals
	     lambda-list positions #'parse-ordinary-optional))
      (setf (values (rest-body result) positions #+cleavir-hack (rest-name result))
	    (parse-rest/body lambda-list positions))
      (setf (values (keys result) positions)
	    (parse-all-keys
	     lambda-list positions #'parse-ordinary-key))
      (setf (values (allow-other-keys result) positions)
	    (parse-allow-other-keys lambda-list positions))
      (setf (values (aux result) positions)
	    (parse-all-aux lambda-list positions))
      ;; We should have run out of parameters now.
      (unless (null (cdr positions))
	(error "something is seriously wrong here"))
      result)))


(defun parse-macro-lambda-list (lambda-list)
  (multiple-value-bind (length structure) (list-structure lambda-list)
    (when (eq structure :circular)
      (error 'lambda-list-must-not-be-circular
	     :code lambda-list))
    (if (eq structure :dotted)
	(progn
	  (when (zerop length)
	    (error 'lambda-list-must-be-list
		   :code lambda-list))
	  (let ((allowed '(&whole &environment &optional)))
	    (check-lambda-list-keywords lambda-list allowed)
	    (let ((positions (compute-keyword-positions lambda-list allowed))
		  (result (make-instance 'lambda-list)))
	      (if (eq (car lambda-list) '&whole)
		  (progn
		    (setf (values (whole result) positions)
			  (parse-whole lambda-list positions))
		    (if (eq (caddr lambda-list) '&environment)
			(progn
			  (setf (values (environment result) positions)
				(parse-environment lambda-list positions))
			  (setf (required result)
				(parse-all-required lambda-list
						    4 (car positions)
						    #'parse-pattern)))
			(setf (required result)
			      (parse-all-required lambda-list
						  2 (car positions)
						  #'parse-pattern))))
		  (if (eq (car lambda-list) '&environment)
		      (progn 
			(setf (values (environment result) positions)
			      (parse-environment lambda-list positions))
			(setf (required result)
			      (parse-all-required lambda-list
						  2 (car positions)
						  #'parse-pattern)))
		      (setf (required result)
			    (parse-all-required lambda-list
						0 (car positions)
						#'parse-pattern))))
	      ;; The environment may follow the required.
	      (when (eq (environment result) :none)
		(setf (values (environment result) positions)
		      (parse-environment lambda-list positions)))
	      (setf (values (optionals result) positions)
		    (parse-all-optionals
		     lambda-list positions #'parse-destructuring-optional))
	      ;; The environment may follow the optionals.
	      (when (eq (environment result) :none)
		(setf (values (environment result) positions)
		      (parse-environment lambda-list positions)))
	      ;; We should have run out of parameters now.
	      (unless (null (cdr positions))
		(error "something is seriously wrong here"))
	      ;; All that remains is to deal with the dotted end
	      ;; of the list.
	      (let ((rest (cdr (last lambda-list))))
		(unless (and (symbolp rest)
			     (not (constantp rest)))
		  (error 'atomic-lambda-list-tail-must-be-variable
			 :code lambda-list))
		(setf (rest-body result) rest))
	      result)))
	(progn
	  (let ((allowed '(&whole &environment &optional &rest &body #+cleavir-hack core:&va-rest
			   &key &allow-other-keys &aux)))
	    (check-lambda-list-keywords lambda-list allowed)
	    (let ((positions (compute-keyword-positions lambda-list allowed))
		  (result (make-instance 'lambda-list)))
	      (if (eq (car lambda-list) '&whole)
		  (progn
		    (setf (values (whole result) positions)
			  (parse-whole lambda-list positions))
		    (if (eq (caddr lambda-list) '&environment)
			(progn
			  (setf (values (environment result) positions)
				(parse-environment lambda-list positions))
			  (setf (required result)
				(parse-all-required lambda-list
						    4 (car positions)
						    #'parse-pattern)))
			(setf (required result)
			      (parse-all-required lambda-list
						  2 (car positions)
						  #'parse-pattern))))
		  (if (eq (car lambda-list) '&environment)
		      (progn 
			(setf (values (environment result) positions)
			      (parse-environment lambda-list positions))
			(setf (required result)
			      (parse-all-required lambda-list
						  2 (car positions)
						  #'parse-pattern)))
		      (setf (required result)
			    (parse-all-required lambda-list
						0 (car positions)
						#'parse-pattern))))
	      ;; The environment may follow the required.
	      (when (eq (environment result) :none)
		(setf (values (environment result) positions)
		      (parse-environment lambda-list positions)))
	      (setf (values (optionals result) positions)
		    (parse-all-optionals
		     lambda-list positions #'parse-destructuring-optional))
	      ;; The environment may follow the optionals.
	      (when (eq (environment result) :none)
		(setf (values (environment result) positions)
		      (parse-environment lambda-list positions)))
	      (setf (values (rest-body result) positions #+cleavir-hack (rest-name result))
		    (parse-rest/body lambda-list positions))
	      ;; The environment may follow the rest/body.
	      (when (eq (environment result) :none)
		(setf (values (environment result) positions)
		      (parse-environment lambda-list positions)))
	      (setf (values (keys result) positions)
		    (parse-all-keys
		     lambda-list positions #'parse-destructuring-key))
	      (setf (values (allow-other-keys result) positions)
		    (parse-allow-other-keys lambda-list positions))
	      ;; The environment may follow the keys.
	      (when (eq (environment result) :none)
		(setf (values (environment result) positions)
		      (parse-environment lambda-list positions)))
	      (setf (values (aux result) positions)
		    (parse-all-aux lambda-list positions))
	      ;; The environment may follow the aux.
	      (when (eq (environment result) :none)
		(setf (values (environment result) positions)
		      (parse-environment lambda-list positions)))
	      ;; We should have run out of parameters now.
	      (unless (null (cdr positions))
		(error "something is seriously wrong here"))
	      result))))))

(defun generate-congruent-lambda-list (method-lambda-list)
  (let* ((parsed-lambda-list
	   (make-instance
	    'lambda-list
	    :required (required method-lambda-list)
	    :optionals (optionals method-lambda-list)
	    :rest (rest-body method-lambda-list)
            #+cleavir-hack :rest-name #+cleavir-hack (rest-name method-lambda-list)
	    :keys (if (eq (keys method-lambda-list) :none)
		      :none
		      '())))
	 (unparsed-lambda-list
	   `(,(required parsed-lambda-list)
	     ,@(if (eq (optionals parsed-lambda-list) :none)
		   '()
		   `(&optional ,@(optionals parsed-lambda-list)))
	     ,@(if (eq (rest-body parsed-lambda-list) :none)
		   '()
		   `(#-cleavir-hack &rest #+cleavir-hack ,(rest-name parsed-lambda-list) ,@(rest-body parsed-lambda-list)))
	     ,@(if (eq (keys parsed-lambda-list) :none)
		   '()
		   `(&key)))))
    (values unparsed-lambda-list
	    parsed-lambda-list)))

;;; ---------- Changes to Code/Cleavir/Generate-AST/generate-ast.lisp

(in-package #:cleavir-generate-ast)

;;; Modified by Christian Schafmeister May 18, 2016 to
;;; generate the proper synonym of &rest that the user provided
;;; using the cleavir-code-utilities:rest-name accessor
(defun process-rest (parsed-lambda-list idspecs body env system)
  (let ((rest (cleavir-code-utilities:rest-body parsed-lambda-list))
        #+cleavir-hack(rest-name (cleavir-code-utilities::rest-name parsed-lambda-list)))
    (if (eq rest :none)
	;; There was no lambda-list keyword &REST or &BODY in this
	;; lambda list.  Just call the function PROCESS-KEYS to create
	;; the AST for the remaining analysis and the modified lambda
	;; list.
	(process-keys parsed-lambda-list idspecs body env system)
	;; This lambda list has the lambda-list &REST or &BODY in it.
	;; It is followed by a single variable to hold the rest of the
	;; arguments.
	(let (;; Create a new environment by augmenting the original
	      ;; one with the &REST parameter variable. 
	      (new-env (augment-environment-with-variable
			rest (first idspecs) env env)))
	  (multiple-value-bind (next-ast next-lexical-parameters)
	      ;; Create the AST and the modified lambda list that
	      ;; results from processing the remaining lambda list. 
	      (process-keys parsed-lambda-list
			    (rest idspecs)
			    body
			    new-env
			    system)
	    (let* (;; We must create a LEXICAL-AST that the
		   ;; implementation-specific argument-parsing code
		   ;; will assign to, so we must give it a name.
		   ;; Ideally, it should have the same name as the
		   ;; parameter variable, but until the code is known
		   ;; to be stable, it is better to distinguish them,
		   ;; so that the have a different appearance in the
		   ;; Graphviz drawing of the AST.  For that reason,
		   ;; we give the LEXICAL-AST the lower-cased version
		   ;; of the name of the parameter variable.
		   (name (make-symbol (string-downcase rest)))
		   (lexical-ast (cleavir-ast:make-lexical-ast name)))
	      (values (set-or-bind-variable
		       rest lexical-ast next-ast new-env system)
		      (list* #+cleavir-hack rest-name #-cleavir-hack'&rest lexical-ast next-lexical-parameters))))))))
