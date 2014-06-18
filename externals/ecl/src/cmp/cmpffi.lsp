;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  CMPFFI --  Foreign functions interface.

;;;;  Copyright (c) 2003, Juan Jose Garcia-Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "COMPILER")

;; ----------------------------------------------------------------------
;; REPRESENTATION TYPES
;;

(defun rep-type-record-unsafe (rep-type)
  (gethash rep-type (machine-rep-type-hash *machine*)))

(defun rep-type-record (rep-type)
  (let ((record (gethash rep-type (machine-rep-type-hash *machine*))))
    (unless record
      (cmperr "Not a valid C type name ~A" rep-type))
    record))

(defun rep-type->lisp-type (name)
  (let ((output (rep-type-record-unsafe name)))
    (cond (output
	   (rep-type-lisp-type output))
	  ((lisp-type-p name) name)
	  (t (error "Unknown representation type ~S" name)))))

(defun lisp-type->rep-type (type)
  (cond
    ;; We expect type = NIL when we have no information. Should be fixed. FIXME!
    ((null type)
     :object)
    ((let ((r (rep-type-record-unsafe type)))
       (and r (rep-type-name r))))
    (t
     ;; Find the most specific type that fits
     (dolist (record (machine-sorted-types *machine*) :object)
       (when (subtypep type (rep-type-lisp-type record))
	 (return-from lisp-type->rep-type (rep-type-name record)))))))

(defun c-number-rep-type-p (rep-type)
  (let ((r (rep-type-record-unsafe rep-type)))
    (and r (rep-type-numberp r))))

(defun c-integer-rep-type-p (rep-type)
  (let ((r (rep-type-record-unsafe rep-type)))
    (and r (rep-type-integerp r))))

(defun c-integer-rep-type-bits (rep-type)
  (let ((r (rep-type-record-unsafe rep-type)))
    (and r (rep-type-bits r))))

(defun c-number-type-p (type)
  (c-number-rep-type-p (lisp-type->rep-type type)))

(defun c-integer-type-p (type)
  (c-integer-rep-type-p (lisp-type->rep-type type)))

(defun c-integer-type-bits (type)
  (c-number-rep-type-bits (lisp-type->rep-type type)))

(defun rep-type->c-name (type)
  (rep-type-c-name (rep-type-record type)))

(defun lisp-type-p (type)
  (subtypep type 'T))

(defun wt-to-object-conversion (loc-rep-type loc)
  (when (and (consp loc) (member (first loc)
				 '(single-float-value
				   double-float-value
				   long-float-value)))
    (wt (third loc)) ;; VV index
    (return-from wt-to-object-conversion))
  (let ((record (rep-type-record loc-rep-type)))
    (unless record
      (cmperr "Cannot coerce C variable of type ~A to lisp object" loc-rep-type))
    (wt (rep-type-to-lisp record) "(" loc ")")))

(defun wt-from-object-conversion (dest-type loc-type rep-type loc)
  (let* ((record (rep-type-record rep-type))
	 (coercer (and record (rep-type-from-lisp record))))
    (unless coercer
      (cmperr "Cannot coerce lisp object to C type ~A" rep-type))
    (wt (if (or (policy-assume-no-errors)
                (subtypep loc-type dest-type))
	    (rep-type-from-lisp-unsafe record)
	    coercer)
	"(" loc ")")))

;; ----------------------------------------------------------------------
;; LOCATIONS and representation types
;;
;; Locations are lisp expressions which represent actual C data. To each
;; location we can associate a representation type, which is the type of
;; the C data. The following routines help in determining these types,
;; and also in moving data from one location to another.

(defun loc-movable-p (loc)
  (if (atom loc)
      t
      (case (first loc)
	((CALL CALL-LOCAL) NIL)
	((C-INLINE) (not (fifth loc))) ; side effects?
	(otherwise t))))

(defun loc-type (loc)
  (cond ((eq loc NIL) 'NULL)
	((var-p loc) (var-type loc))
        ((vv-p loc) (vv-type loc))
	((numberp loc) (lisp-type->rep-type (type-of loc)))
	((atom loc) 'T)
	(t
	 (case (first loc)
	   (FIXNUM-VALUE 'FIXNUM)
	   (CHARACTER-VALUE (type-of (code-char (second loc))))
	   (DOUBLE-FLOAT-VALUE 'DOUBLE-FLOAT)
	   (SINGLE-FLOAT-VALUE 'SINGLE-FLOAT)
	   (LONG-FLOAT-VALUE 'LONG-FLOAT)
	   (C-INLINE (let ((type (first (second loc))))
                       (cond ((and (consp type) (eq (first type) 'VALUES)) T)
                             ((lisp-type-p type) type)
                             (t (rep-type->lisp-type type)))))
	   (BIND (var-type (second loc)))
	   (LCL (or (third loc) T))
	   (THE (second loc))
	   (CALL-NORMAL (fourth loc))
	   (otherwise T)))))

(defun loc-representation-type (loc)
  (cond ((member loc '(NIL T)) :object)
	((var-p loc) (var-rep-type loc))
        ((vv-p loc) :object)
	((numberp loc) (lisp-type->rep-type (type-of loc)))
        ((eq loc 'TRASH) :void)
	((atom loc) :object)
	(t
	 (case (first loc)
	   (FIXNUM-VALUE :fixnum)
	   (CHARACTER-VALUE (if (<= (second loc) 255) :unsigned-char :wchar))
	   (DOUBLE-FLOAT-VALUE :double)
	   (SINGLE-FLOAT-VALUE :float)
	   (LONG-FLOAT-VALUE :long-double)
	   (C-INLINE (let ((type (first (second loc))))
                       (cond ((and (consp type) (eq (first type) 'VALUES)) :object)
                             ((lisp-type-p type) (lisp-type->rep-type type))
                             (t type))))
	   (BIND (var-rep-type (second loc)))
	   (LCL (lisp-type->rep-type (or (third loc) T)))
           ((JUMP-TRUE JUMP-FALSE) :bool)
	   (THE (loc-representation-type (third loc)))
	   (otherwise :object)))))

(defun wt-coerce-loc (dest-rep-type loc)
  (setq dest-rep-type (lisp-type->rep-type dest-rep-type))
  ;(print dest-rep-type)
  ;(print loc)
  (let* ((dest-type (rep-type->lisp-type dest-rep-type))
	 (loc-type (loc-type loc))
	 (loc-rep-type (loc-representation-type loc)))
    (labels ((coercion-error ()
	       (cmpwarn "Unable to coerce lisp object from type (~S,~S)~%~
			to C/C++ type (~S,~S)"
                        loc-type loc-rep-type dest-type dest-rep-type))
	     (ensure-valid-object-type (a-lisp-type)
	       (when (subtypep `(AND ,loc-type ,a-lisp-type) NIL)
		 (coercion-error))))
      (when (eq dest-rep-type loc-rep-type)
	(wt loc)
	(return-from wt-coerce-loc))
      (case dest-rep-type
	((:char :unsigned-char :wchar)
	 (case loc-rep-type
	   ((:char :unsigned-char :wchar)
	    (wt "(" (rep-type->c-name dest-rep-type) ")(" loc ")"))
	   ((:object)
	    (ensure-valid-object-type dest-type)
	    (wt-from-object-conversion dest-type loc-type dest-rep-type loc))
	   (otherwise
	    (coercion-error))))
	((:float :double :long-double)
	 (cond
	   ((c-number-rep-type-p loc-rep-type)
	    (wt "(" (rep-type->c-name dest-rep-type) ")(" loc ")"))
	   ((eq loc-rep-type :object)
	    ;; We relax the check a bit, because it is valid in C to coerce
	    ;; between floats of different types.
	    (ensure-valid-object-type 'FLOAT)
	    (wt-from-object-conversion dest-type loc-type dest-rep-type loc))
	   (t
	    (coercion-error))))
	((:bool)
	 (cond
	   ((c-number-rep-type-p loc-rep-type)
	    (wt "1"))
	   ((eq loc-rep-type :object)
	    (wt "(" loc ")!=ECL_NIL"))
	   (t
	    (coercion-error))))
	((:object)
	 (case loc-rep-type
	   ((:int-sse-pack :float-sse-pack :double-sse-pack)
            (when (>= (cmp-env-optimization 'speed) 1)
              (cmpwarn-style "Boxing a value of type ~S - performance degraded."
                             loc-rep-type))))
	 (wt-to-object-conversion loc-rep-type loc))
	((:pointer-void)
	 (case loc-rep-type
	   ((:object)
	    (wt-from-object-conversion dest-type loc-type dest-rep-type loc))
	   ((:cstring)
	    (wt "(char *)(" loc ")"))
	   (otherwise
	    (coercion-error))))
	((:cstring)
	 (coercion-error))
	((:char*)
	 (case loc-rep-type
	   ((:object)
	    (wt "ecl_base_string_pointer_safe(" loc ")"))
	   ((:pointer-void)
	    (wt "(char *)(" loc ")"))
	   (otherwise
	    (coercion-error))))
	((:int-sse-pack :float-sse-pack :double-sse-pack)
	 (case loc-rep-type
	   ((:object)
	    (wt-from-object-conversion 'ext:sse-pack loc-type dest-rep-type loc))
           ;; Implicitly cast between SSE subtypes
           ((:int-sse-pack :float-sse-pack :double-sse-pack)
            (wt (ecase dest-rep-type
                  (:int-sse-pack (ecase loc-rep-type
                                   (:float-sse-pack "_mm_castps_si128")
                                   (:double-sse-pack "_mm_castpd_si128")))
                  (:float-sse-pack (ecase loc-rep-type
                                     (:int-sse-pack "_mm_castsi128_ps")
                                     (:double-sse-pack "_mm_castpd_ps")))
                  (:double-sse-pack (ecase loc-rep-type
                                      (:int-sse-pack "_mm_castsi128_pd")
                                      (:float-sse-pack "_mm_castps_pd"))))
                "(" loc ")"))
	   (otherwise
	    (coercion-error))))
	(t
	 ;; At this point we only have coercions to integers
	 (cond
	   ((not (c-integer-rep-type-p dest-rep-type))
	    (coercion-error))
	   ((c-number-rep-type-p loc-rep-type)
	    (wt "(" (rep-type->c-name dest-rep-type) ")(" loc ")"))
	   ((eq :object loc-rep-type)
	    (ensure-valid-object-type dest-type)
	    (wt-from-object-conversion dest-type loc-type dest-rep-type loc))
	   (t
	    (coercion-error))))))))

;;; ----------------------------------------------------------------------
;;; C/C++ DECLARATIONS AND HEADERS
;;;
;;; All lines from CLINES statements are grouped at the beginning of the header
;;; Notice that it does not make sense to guarantee that c-lines statements
;;; are produced in-between the function definitions, because two functions
;;; might be collapsed into one, or we might not produce that function at all
;;; and rather inline it.
;;;
(defun c1clines (args)
  (unless (every #'stringp args)
    (cmperr "The argument to CLINES, ~s, is not a list of strings." args))
  (setf *clines-string-list* (nconc *clines-string-list* (copy-list args)))
  '(progn))

(defun output-clines (output-stream)
  (flet ((parse-one-string (s output-stream)
           (with-input-from-string (stream s)
             (loop for c = (read-char stream nil nil)
                while c
                do (if (eq c #\@)
                       (let ((object (handler-case (read stream)
                                       (serious-condition (c)
                                         (cmperr "Unable to parse FFI:CLINES string~& ~S"
                                                 s)))))
                         (let ((*compiler-output1* output-stream))
                           (wt (add-object object :permanent t))))
                       (write-char c output-stream))))))
    (loop for s in *clines-string-list*
       do (terpri output-stream)
       do (if (find #\@ s)
              (parse-one-string s output-stream)
              (write-string s output-stream)))
    (terpri output-stream)
    (setf *clines-string-list* nil)))

;; ----------------------------------------------------------------------
;; C/C++ INLINE CODE
;;

(defun c1c-inline (args)
  ;; We are on the safe side by assuming that the form has side effects
  (destructuring-bind (arguments arg-types output-type c-expression
				 &rest rest
				 &key (side-effects t) one-liner
				 &aux output-rep-type)
      args
    (unless (= (length arguments) (length arg-types))
      (cmperr "In a C-INLINE form the number of declare arguments and the number of supplied ones do not match:~%~S"
	      `(C-INLINE ,@args)))
    ;; We cannot handle :cstrings as input arguments. :cstrings are
    ;; null-terminated strings, but not all of our lisp strings will
    ;; be null terminated. In particular, those with a fill pointer
    ;; will not.
    (let ((ndx (position :cstring arg-types)))
      (when ndx
	(let* ((var (gensym))
               (arguments (copy-list arguments))
	       (value (elt arguments ndx)))
	  (setf (elt arguments ndx) var
		(elt arg-types ndx) :char*)
	  (return-from c1c-inline
	    (c1expr
	     `(ffi::with-cstring (,var ,value)
	       (c-inline ,arguments ,arg-types ,output-type ,c-expression
		,@rest)))))))
    ;; Find out the output types of the inline form. The syntax is rather relaxed
    ;; 	output-type = lisp-type | c-type | (values {lisp-type | c-type}*)
    (flet ((produce-type-pair (type)
	     (if (lisp-type-p type)
		 (cons type (lisp-type->rep-type type))
		 (cons (rep-type->lisp-type type) type))))
      (cond ((eq output-type ':void)
	     (setf output-rep-type '()
		   output-type 'NIL))
	    ((equal output-type '(VALUES &REST t))
	     (setf output-rep-type '((VALUES &REST t))))
	    ((and (consp output-type) (eql (first output-type) 'VALUES))
	     (let ((x (mapcar #'produce-type-pair (rest output-type))))
	       (setf output-rep-type (mapcar #'cdr x)
		     output-type `(VALUES ,@(mapcar #'car x)))))
	    (t
	     (let ((x (produce-type-pair output-type)))
	       (setf output-type (car x)
		     output-rep-type (list (cdr x)))))))
    (unless (and (listp arguments)
		 (listp arg-types)
		 (stringp c-expression))
      (cmperr "C-INLINE: syntax error in ~S"
	      (list* 'c-inline args)))
    (unless (= (length arguments)
	       (length arg-types))
      (cmperr "C-INLINE: wrong number of arguments in ~S"
	      (list* 'c-inline args)))
    (let* ((arguments (mapcar #'c1expr arguments))
	   (form (make-c1form* 'C-INLINE :type output-type
			       :side-effects side-effects
			       :args arguments arg-types
			       output-rep-type
			       c-expression
			       side-effects
			       one-liner)))
      (loop for form in arguments
	 when (eq (c1form-name form) 'VAR)
	 do (let ((var (c1form-arg 0 form)))
	      (add-to-set-nodes var form)))
      form)))

(defun c1c-progn (arguments)
  (let* ((variables (mapcar #'c1vref (pop arguments)))
	 (statements (loop for form in arguments
			collect (if (stringp form)
				    form
				    (c1expr form))))
	 (form (make-c1form* 'FFI:C-PROGN :type NIL
			     :side-effects t
			     :args variables statements)))
    (add-to-set-nodes-of-var-list variables form)
    form))

(defun c2c-progn (c1form variables statements)
  (loop with *destination* = 'TRASH
     for form in statements
     do (cond ((stringp form)
	       (wt-nl)
	       (wt-c-inline-loc :void form variables
				t ; side effects
				nil) ; no output variables
	       )
	      (t
	       (c2expr* form)))
     finally (unwind-exit nil)))

(defun produce-inline-loc (inlined-arguments arg-types output-rep-type
			   c-expression side-effects one-liner)
  (let* (args-to-be-saved
	 coerced-arguments)
    ;; If the expression begins with @[0-9a-z]*, this means we are
    ;; saving some variables.
    (when (and (> (length c-expression) 1)
	       (eq (char c-expression 0) #\@))
      (do ((ndx 1 (1+ ndx)))
	  ((>= ndx (length c-expression)))
	(let ((c (char c-expression ndx)))
	  (when (eq c #\;)
	    (setf c-expression (subseq c-expression (1+ ndx)))
	    (return))
	  (unless (alphanumericp c)
	    (setf args-to-be-saved nil)
	    (return))
	  (push (- (char-code c) (char-code #\0))
		args-to-be-saved))))

    (setf coerced-arguments (coerce-locs inlined-arguments arg-types args-to-be-saved))
    ;;(setf output-rep-type (lisp-type->rep-type output-rep-type))

    ;; If the form does not output any data, and there are no side
    ;; effects, try to omit it.
    (when (null output-rep-type)
      (if side-effects
	  (progn
	    (wt-nl)
	    (wt-c-inline-loc output-rep-type c-expression coerced-arguments t nil)
	    (when one-liner (wt ";")))
	  (cmpnote "Ignoring form ~S" c-expression))
      (return-from produce-inline-loc NIL))

    ;; If the form is a one-liner, we can simply propagate this expression until the
    ;; place where the value is used.
    (when one-liner
      (return-from produce-inline-loc
	`(C-INLINE ,output-rep-type ,c-expression ,coerced-arguments ,side-effects
                   ,(if (equalp output-rep-type '((VALUES &REST T)))
                        'VALUES NIL))))

    ;; If the output is a in the VALUES vector, just write down the form and output
    ;; the location of the data.
    (when (equalp output-rep-type '((VALUES &REST T)))
      (wt-c-inline-loc output-rep-type c-expression coerced-arguments side-effects
                       'VALUES)
      (return-from produce-inline-loc 'VALUES))

    ;; Otherwise we have to set up variables for holding the output.
    (flet ((make-output-var (type)
	     (let ((var (make-lcl-var :rep-type type)))
	       (wt-nl (rep-type->c-name type) " " var ";")
	       var)))
      (open-inline-block)
      (let ((output-vars (mapcar #'make-output-var output-rep-type)))
	(wt-c-inline-loc output-rep-type c-expression coerced-arguments side-effects output-vars)
	(cond ((= (length output-vars) 1)
	       (first output-vars))
	      (t
	       (loop for v in output-vars
		     for i from 0
		     do (let ((*destination* `(VALUE ,i))) (set-loc v)))
	       (wt "cl_env_copy->nvalues=" (length output-vars) ";")
	       'VALUES))))))

(defun c2c-inline (c1form arguments &rest rest)
  (declare (ignore c1form))
  (let ((*inline-blocks* 0)
        (*temp* *temp*))
    (unwind-exit (apply #'produce-inline-loc (inline-args arguments) rest))
    (close-inline-blocks)))

(defun coerce-locs (inlined-args &optional types args-to-be-saved)
  ;; INLINED-ARGS is a list of (TYPE LOCATION) produced by the
  ;; inline code. ARGS-TO-BE-SAVED is a positional list created by
  ;; C-INLINE, instructing that the value should be saved in a temporary
  ;; variable. Finally, TYPES is a list of destination types, to which
  ;; the former values are coerced. The destination types can be
  ;;	- A lisp type (:OBJECT, :FINXUM, etc)
  ;;	- A machine representation type (T, INTEGER, etc)
  (loop with block-opened = nil
     for (lisp-type loc) in inlined-args
     for type in (or types '#1=(:object . #1#))
     for i from 0
     for rep-type = (lisp-type->rep-type type)
     collect
       (cond ((and args-to-be-saved
		   (member i args-to-be-saved :test #'eql)
		   (not (loc-movable-p loc)))
	      (let ((lcl (make-lcl-var :rep-type rep-type)))
		(wt-nl)
		(unless block-opened
		  (setf block-opened t)
		  (open-inline-block))
		(wt (rep-type->c-name rep-type) " " lcl "= ")
		(wt-coerce-loc rep-type loc)
		(wt ";")
		lcl))
	     ((equal rep-type (loc-representation-type loc))
	      loc)
	     (t
	      `(COERCE-LOC ,rep-type ,loc)))))

(defun wt-c-inline-loc (output-rep-type c-expression coerced-arguments side-effects output-vars)
  (with-input-from-string (s c-expression)
    (when (and output-vars (not (eq output-vars 'VALUES)))
      (wt-nl))
    (do ((c (read-char s nil nil)
	    (read-char s nil nil)))
	((null c))
      (case c
	(#\@
	 (let ((object (read s)))
	   (cond ((and (consp object) (equal (first object) 'RETURN))
		  (if (eq output-vars 'VALUES)
		      (cmperr "User @(RETURN ...) in a C-INLINE form with no output values")
		      (let ((ndx (or (second object) 0))
			    (l (length output-vars)))
			(if (< ndx l)
			    (wt (nth ndx output-vars))
			  (cmperr "Used @(RETURN ~D) in a C-INLINE form with ~D output values"
				  ndx l)))))
		 (t
		  (when (and (consp object) (eq (first object) 'QUOTE))
		    (setq object (second object)))
		  (wt (add-object object :permanent t))))))
	(#\#
	 (let* ((k (read-char s))
		(next-char (peek-char nil s nil nil))
		(index (digit-char-p k 36)))
	   (cond ((eq k #\#)
                  (wt #\#))
                 ((or (null index) (and next-char (alphanumericp next-char)))
		  (wt #\# k))
		 ((< index (length coerced-arguments))
		  (wt (nth index coerced-arguments)))
		 (t
		  (cmperr "C-INLINE: Variable code exceeds number of arguments")))))
	(otherwise
	 (write-char c *compiler-output1*))))))

(defun c-inline-safe-string (constant-string)
  ;; Produce a text representation of a string that can be used
  ;; in a C-INLINE form, without triggering the @ or # escape
  ;; characters
  (c-filtered-string
   (concatenate 'string
		(loop for c across constant-string
		   when (member c '(#\# #\@))
		   collect c
		   collect c))))
