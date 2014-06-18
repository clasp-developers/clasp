;;; -*- Mode:Common-Lisp; Package:XLIB; Syntax:COMMON-LISP; Base:10; Lowercase:T -*-

;; RESOURCE - Lisp version of XLIB's Xrm resource manager

;;;
;;;			 TEXAS INSTRUMENTS INCORPORATED
;;;				  P.O. BOX 2909
;;;			       AUSTIN, TEXAS 78769
;;;
;;; Copyright (C) 1987 Texas Instruments Incorporated.
;;;
;;; Permission is granted to any individual or institution to use, copy, modify,
;;; and distribute this software, provided that this complete copyright and
;;; permission notice is maintained, intact, in all copies and supporting
;;; documentation.
;;;
;;; Texas Instruments Incorporated provides this software "as is" without
;;; express or implied warranty.
;;;

(in-package :xlib)

;; The C version of this uses a 64 entry hash table at each entry.
;; Small hash tables lose in Lisp, so we do linear searches on lists.

(defstruct (resource-database (:copier nil) (:predicate nil)
			      (:print-function print-resource-database)
			      (:constructor make-resource-database-internal)
			      #+explorer (:callable-constructors nil)
			      )
  (name nil :type stringable :read-only t)
  (value nil)
  (tight nil :type list) ;; List of resource-database
  (loose nil :type list) ;; List of resource-database
  )

(defun print-resource-database (database stream depth)
  (declare (type resource-database database)
	   (ignore depth))
  (print-unreadable-object (database stream :type t)
    (write-string (string (resource-database-name database)) stream)
    (when (resource-database-value database)
      (write-string " " stream)
      (prin1 (resource-database-value database) stream))))

;; The value slot of the top-level resource-database structure is used for a
;; time-stamp.

(defun make-resource-database ()
  ;; Make a resource-database with initial timestamp of 0
  (make-resource-database-internal :name "Top-Level" :value 0))

(defun resource-database-timestamp (database)
  (declare (type resource-database database))
  (resource-database-value database))

(defun incf-resource-database-timestamp (database)
  ;; Increment the timestamp
  (declare (type resource-database database))
  (let ((timestamp (resource-database-value database)))
    (setf (resource-database-value database)
	  (if (= timestamp most-positive-fixnum)
	      most-negative-fixnum
	    (1+ timestamp)))))

;; DEBUG FUNCTION  (not exported)
(defun print-db (entry &optional (level 0) type)
  ;; Debug function to print a resource database
  (format t "~%~v@t~s~:[~; *~]~@[ Value ~s~]"
	  level
	  (resource-database-name entry)
	  (eq type 'loose)
	  (resource-database-value entry))
  (when (resource-database-tight entry)
    (dolist (tight (resource-database-tight entry))
      (print-db tight (+ 2 level) 'tight)))
  (when (resource-database-loose entry)
    (dolist (loose (resource-database-loose entry))
      (print-db loose (+ 2 level) 'loose))))

;; DEBUG FUNCTION
#+comment
(defun print-search-table (table)
  (terpri)
  (dolist (dbase-list table)
    (format t "~%~s" dbase-list)
    (dolist (db dbase-list)
      (print-db db)
      (dolist (dblist table)
	(unless (eq dblist dbase-list)
	  (when (member db dblist)
	    (format t "  duplicate at ~s" db))))
      )))

;;
;; If this is true, resource symbols will be compared in a case-insensitive
;; manner, and converting a resource string to a keyword will uppercaseify it.
;;
(defparameter *uppercase-resource-symbols* nil)

(defun resource-key (stringable)
  ;; Ensure STRINGABLE is a keyword.
  (declare (type stringable stringable))
  (etypecase stringable
    (symbol
      (if (keywordp (the symbol stringable))
	  stringable
	  (kintern (symbol-name (the symbol stringable)))))
    (string
      (if *uppercase-resource-symbols*
	  (setq stringable (#-allegro string-upcase #+allegro correct-case
			    (the string stringable))))
      (kintern (the string stringable)))))

(defun stringable-equal (a b)
  ;; Compare two stringables.
  ;; Ignore case when comparing to a symbol.
  (declare (type stringable a b))
  (declare (clx-values generalized-boolean))
  (etypecase a
    (string
      (etypecase b
	(string
	  (string= (the string a) (the string b)))
	(symbol
	  (if *uppercase-resource-symbols*
	      (string-equal (the string a)
			    (the string (symbol-name (the symbol b))))
	      (string= (the string a)
		       (the string (symbol-name (the symbol b))))))))
    (symbol
      (etypecase b
	(string
	  (if *uppercase-resource-symbols*
	      (string-equal (the string (symbol-name (the symbol a)))
			    (the string b))
	      (string= (the string (symbol-name (the symbol a)))
		       (the string b))))
	(symbol
	  (string= (the string (symbol-name (the symbol a)))
		   (the string (symbol-name (the symbol b)))))))))


;;;-----------------------------------------------------------------------------
;;; Add/delete resource

(defun add-resource (database name-list value)
  ;; name-list is a list of either strings or symbols. If a symbol, 
  ;; case-insensitive comparisons will be used, if a string,
  ;; case-sensitive comparisons will be used.  The symbol '* or
  ;; string "*" are used as wildcards, matching anything or nothing.
  (declare (type resource-database database)
	   (type (clx-list stringable) name-list)
	   (type t value))
  (unless value (error "Null resource values are ignored"))
  (incf-resource-database-timestamp database)
  (do* ((list name-list (cdr list))
	(name (car list) (car list))
	(node database)
	(loose-p nil))
       ((endp list)
	(setf (resource-database-value node) value))
    ;; Key is the first name that isn't *
    (if (stringable-equal name "*")
	(setq loose-p t)
      ;; find the entry associated with name
      (progn
	(do ((entry (if loose-p
			(resource-database-loose node)
		      (resource-database-tight node))
		    (cdr entry)))
	    ((endp entry)
	     ;; Entry not found - create a new one
	     (setq entry (make-resource-database-internal :name name))
	     (if loose-p
		 (push entry (resource-database-loose node))
	       (push entry (resource-database-tight node)))
	     (setq node entry))
	  (when (stringable-equal name (resource-database-name (car entry)))
	    ;; Found entry - use it
	    (return (setq node (car entry)))))
	(setq loose-p nil)))))


(defun delete-resource (database name-list)
  (declare (type resource-database database)
	   (type list name-list))
  (incf-resource-database-timestamp database)
  (delete-resource-internal database name-list))

(defun delete-resource-internal (database name-list)
  (declare (type resource-database database)
	   (type (clx-list stringable) name-list))
  (do* ((list name-list (cdr list))
	(string (car list) (car list))
	(node database)
	(loose-p nil))
       ((endp list) nil)
    ;; Key is the first name that isn't *
    (if (stringable-equal string "*")
	(setq loose-p t)
      ;; find the entry associated with name
      (progn 
	(do* ((first-entry (if loose-p
			       (resource-database-loose node)
			     (resource-database-tight node)))
	      (entry-list first-entry (cdr entry-list))
	      (entry (car entry-list) (car entry-list)))
	     ((endp entry-list)
	      ;; Entry not found - exit
	      (return-from delete-resource-internal nil))
	  (when (stringable-equal string (resource-database-name entry))
	    (when (cdr list) (delete-resource-internal entry (cdr list)))
	    (when (and (null (resource-database-loose entry))
		       (null (resource-database-tight entry)))
	      (if loose-p
		  (setf (resource-database-loose node)
			(delete entry (resource-database-loose node)
				:test #'eq :count 1))
		(setf (resource-database-tight node)
		      (delete entry (resource-database-tight node)
			      :test #'eq :count 1))))
	    (return-from delete-resource-internal t)))
	(setq loose-p nil)))))

;;;-----------------------------------------------------------------------------
;;; Get Resource

(defun get-resource (database value-name value-class full-name full-class)
  ;; Return the value of the resource in DATABASE whose partial name
  ;; most closely matches (append full-name (list value-name)) and
  ;;                      (append full-class (list value-class)).
  (declare (type resource-database database)
	   (type stringable value-name value-class)
	   (type (clx-list stringable) full-name full-class))
  (declare (clx-values value))
  (let ((names (append full-name (list value-name)))
	(classes (append full-class (list value-class))))
    (let* ((result (get-entry (resource-database-tight database)
			      (resource-database-loose database)
			      names classes)))
      (when result
	(resource-database-value result)))))

(defun get-entry-lookup (table name names classes)
  (declare (type list table names classes)
	   (symbol name))
  (dolist (entry table)
    (declare (type resource-database entry))
    (when (stringable-equal name (resource-database-name entry))
      (if (null (cdr names))
	  (return entry)
	(let ((result (get-entry (resource-database-tight entry)
				 (resource-database-loose entry)
				 (cdr names) (cdr classes))))
	  (declare (type (or null resource-database) result))
	  (when result
	    (return result)
	    ))))))

(defun get-entry (tight loose names classes &aux result)
  (declare (type list tight loose names classes))
  (let ((name (car names))
	(class (car classes)))
    (declare (type symbol name class))
    (cond ((and tight
		(get-entry-lookup tight name names classes)))
	  ((and loose
		(get-entry-lookup loose name names classes)))
	  ((and tight
		(not (stringable-equal name class))
		(get-entry-lookup tight class names classes)))
	  ((and loose
		(not (stringable-equal name class))
		(get-entry-lookup loose class names classes)))	
	  (loose
	   (loop
	     (pop names) (pop classes)
	     (unless (and names classes) (return nil))
	     (setq name (car names)
		   class (car classes))
	     (when (setq result (get-entry-lookup loose name names classes))
	       (return result))
	     (when (and (not (stringable-equal name class))
			(setq result
			      (get-entry-lookup loose class names classes)))
	       (return result))
	     )))))


;;;-----------------------------------------------------------------------------
;;; Get-resource with search-table

(defun get-search-resource (table name class)
  ;; (get-search-resource (get-search-table database full-name full-class) 
  ;;                      value-name value-class)
  ;; is equivalent to 
  ;; (get-resource database value-name value-class full-name full-class)
  ;; But since most of the work is done by get-search-table,
  ;; get-search-resource is MUCH faster when getting several resources with
  ;; the same full-name/full-class
  (declare (type list table)
	   (type stringable name class))
  (let ((do-class (and class (not (stringable-equal name class)))))
    (dolist (dbase-list table)
      (declare (type list dbase-list))
      (dolist (dbase dbase-list)
	(declare (type resource-database dbase))
	(when (stringable-equal name (resource-database-name dbase))
	  (return-from get-search-resource
	    (resource-database-value dbase))))
      (when do-class
	(dolist (dbase dbase-list)
	  (declare (type resource-database dbase))
	  (when (stringable-equal class (resource-database-name dbase))
	    (return-from get-search-resource
	      (resource-database-value dbase))))))))

(defvar *get-table-result*)

(defun get-search-table (database full-name full-class)
  ;; Return a search table for use with get-search-resource.
  (declare (type resource-database database)
	   (type (clx-list stringable) full-name full-class))
  (declare (clx-values value))
  (let* ((tight (resource-database-tight database))
	 (loose (resource-database-loose database))
	 (result (cons nil nil))
	 (*get-table-result* result))
    (declare (type list tight loose)
	     (type cons result))
    (when (or tight loose)
      (when full-name
	(get-tables tight loose full-name full-class))

      ;; Pick up bindings of the form (* name). These are the elements of
      ;; top-level loose without further tight/loose databases.
      ;;
      ;; (Hack: these bindings belong in ANY search table, so recomputing them
      ;; is a drag.  True fix involves redesigning entire lookup
      ;; data-structure/algorithm.)
      ;;
      (let ((universal-bindings
	      (remove nil loose :test-not #'eq
		      :key #'(lambda (database)
			       (or (resource-database-tight database)
				   (resource-database-loose database))))))
	(when universal-bindings
	  (setf (cdr *get-table-result*) (list universal-bindings)))))
    (cdr result)))

(defun get-tables-lookup (dbase name names classes)
  (declare (type list dbase names classes)
	   (type symbol name))
  (declare (optimize speed))
  (dolist (entry dbase)
    (declare (type resource-database entry))
    (when (stringable-equal name (resource-database-name entry))
      (let ((tight (resource-database-tight entry))
	    (loose (resource-database-loose entry)))
	(declare (type list tight loose))
	(when (or tight loose)
	  (if (cdr names)
	      (get-tables tight loose (cdr names) (cdr classes))
	    (when tight
	      (let ((result *get-table-result*))
		;; Put tight at end of *get-table-result*
		(setf (cdr result)
		      (setq *get-table-result* (cons tight nil))))))
	  (when loose
	    (let ((result *get-table-result*))
	      ;; Put loose at end of *get-table-result*
	      (setf (cdr result)
		    (setq *get-table-result* (cons loose nil))))))))))

(defun get-tables (tight loose names classes)
  (declare (type list tight loose names classes))
  (let ((name (car names))
	(class (car classes)))
    (declare (type symbol name class))
    (when tight
      (get-tables-lookup tight name names classes))
    (when loose
      (get-tables-lookup loose name names classes))
    (when (and tight (not (stringable-equal name class)))
      (get-tables-lookup tight class names classes))
    (when (and loose (not (stringable-equal name class)))
      (get-tables-lookup loose class names classes))
    (when loose
      (loop
	(pop names) (pop classes)
	(unless (and names classes) (return nil))
	(setq name (car names)
	      class (car classes))
	(get-tables-lookup loose name names classes)
	(unless (stringable-equal name class)
	  (get-tables-lookup loose class names classes))
	))))


;;;-----------------------------------------------------------------------------
;;; Utility functions

(defun map-resource (database function &rest args)
  ;; Call FUNCTION on each resource in DATABASE.
  ;; FUNCTION is called with arguments (name-list value . args)
  (declare (type resource-database database)
	   (type (function (list t &rest t) t) function)
	   #+clx-ansi-common-lisp
	   (dynamic-extent function)
	   #+(and lispm (not clx-ansi-common-lisp))
	   (sys:downward-funarg function)
	   (dynamic-extent args))
  (declare (clx-values nil))
  (labels ((map-resource-internal (database function args name)
	     (declare (type resource-database database)
		      (type (function (list t &rest t) t) function)
		      (type list name)
		      #+clx-ansi-common-lisp
		      (dynamic-extent function)
		      #+(and lispm (not clx-ansi-common-lisp))
		      (sys:downward-funarg function))		      
	     (let ((tight (resource-database-tight database))
		   (loose (resource-database-loose database)))
	       (declare (type list tight loose))
	       (dolist (resource tight)
		 (declare (type resource-database resource))
		 (let ((value (resource-database-value resource))
		       (name (append
			       name
			       (list (resource-database-name resource)))))
		   (if value
		       (apply function name value args)
		     (map-resource-internal resource function args name))))
	       (dolist (resource loose)
		 (declare (type resource-database resource))
		 (let ((value (resource-database-value resource))
		       (name (append
			       name
			       (list "*" (resource-database-name resource)))))
		   (if value
		       (apply function name value args)
		     (map-resource-internal resource function args name)))))))
    (map-resource-internal database function args nil)))

(defun merge-resources (database with-database)
  (declare (type resource-database database with-database))
  (declare (clx-values resource-database))
  (map-resource
    database
    #'(lambda (name value database)
	(add-resource database name value))
    with-database)
  with-database)

(defun char-memq (key char)
  ;; Used as a test function for POSITION
  (declare (type base-char char))
  (member char key))

(defmacro resource-with-open-file ((stream pathname &rest options) &body body)
  ;; Private WITH-OPEN-FILE, which, when pathname is a stream, uses it as the
  ;; stream
  (let ((abortp (gensym))
	(streamp (gensym)))
    `(let* ((,abortp t)
	    (,streamp (streamp pathname))
	    (,stream (if ,streamp pathname (open ,pathname ,@options))))
       (unwind-protect
	   (multiple-value-prog1
	     (progn ,@body)
	     (setq ,abortp nil))
	 (unless ,streamp
	   (close stream :abort ,abortp))))))

(defun read-resources (database pathname &key key test test-not)
  ;; Merges resources from a file in standard X11 format with DATABASE.
  ;; KEY is a function used for converting value-strings, the default is
  ;; identity.  TEST and TEST-NOT are predicates used for filtering
  ;; which resources to include in the database.  They are called with
  ;; the name and results of the KEY function.
  (declare (type resource-database database)
	   (type (or pathname string stream) pathname)
	   (type (or null (function (string) t)) key)
	   (type (or null (function (list t) generalized-boolean))
                 test test-not))
  (declare (clx-values resource-database))
  (resource-with-open-file (stream pathname)
    (loop
      (let ((string (read-line stream nil :eof)))
	(declare (type (or string keyword) string))
	(when (eq string :eof) (return database))
	(let* ((end (length string))
	       (i (position '(#\tab #\space) string
			    :test-not #'char-memq :end end))
	       (term nil))
	  (declare (type array-index end)
		   (type (or null array-index) i term))
	  (when i ;; else blank line
	    (case (char string i)
	      (#\! nil)  ;; Comment - skip
	      ;;(#.(card8->char 0) nil) ;; terminator for C strings - skip
	      (#\#       ;; Include
	       (setq term (position '(#\tab #\space) string :test #'char-memq
				    :start i :end end))
	       (when (string-equal string "#INCLUDE" :start1 i :end1 term) 
		 (let ((path (merge-pathnames
			      (string-trim '(#\tab #\space #\")
					   (subseq string (1+ term)))
			      (truename stream))))
		   (read-resources database path
				   :key key :test test :test-not test-not))))
	      (otherwise
	       (multiple-value-bind (name-list value)
		   (parse-resource string i end)
		 (when name-list 
		   (when key (setq value (funcall key value)))
		   (when
		     (cond (test (funcall test name-list value))
			   (test-not (not (funcall test-not name-list value)))
			   (t t))
		     (add-resource database name-list value))))))))))))

(defun parse-resource (string &optional (start 0) end)
  ;; Parse a resource specfication string into a list of names and a value
  ;; string
  (declare (type string string)
	   (type array-index start)
	   (type (or null array-index) end))
  (declare (clx-values name-list value))
  (do ((i start)
       (end (or end (length string)))
       (term)
       (name-list))
      ((>= i end))
    (declare (type array-index end)
	     (type (or null array-index) i term))
    (setq term (position '(#\. #\* #\:) string
			 :test #'char-memq :start i :end end))
    (case (and term (char string term))
      ;; Name seperator
      (#\. (when (> term i)
	     (push (subseq string i term) name-list)))
      ;; Wildcard seperator
      (#\* (when (> term i)
	     (push (subseq string i term) name-list))
	   (push '* name-list))
      ;; Value separator
      (#\:
       (push (subseq string i term) name-list)
       (return
	 (values
	   (nreverse name-list)
	   (string-trim '(#\tab #\space) (subseq string (1+ term))))))
      (otherwise
	(return
	  (values
	    (nreverse name-list)
	    (subseq string i term)))))
    (setq i (1+ term))))

(defun write-resources (database pathname &key write test test-not)
  ;; Write resources to PATHNAME in the standard X11 format.
  ;; WRITE is a function used for writing values, the default is #'princ
  ;; TEST and TEST-NOT are predicates used for filtering which resources
  ;; to include in the database.  They are called with the name and value.
  (declare (type resource-database database)
	   (type (or pathname string stream) pathname)
	   (type (or null (function (string stream) t)) write)
	   (type (or null (function (list t) generalized-boolean))
                 test test-not))
  (resource-with-open-file (stream pathname :direction :output)
    (map-resource
      database
      #'(lambda (name-list value stream write test test-not)
	  (when
	    (cond (test (funcall test name-list value))
		  (test-not (not (funcall test-not name-list value)))
		  (t t))
	    (let ((previous (car name-list)))
	      (princ previous stream)
	      (dolist (name (cdr name-list))
		(unless (or (stringable-equal name "*")
			    (stringable-equal previous "*"))
		  (write-char #\. stream))
		(setq previous name)
		(princ name stream)))
	    (write-string ":	" stream)
	    (funcall write value stream)
	    (terpri stream)))
      stream (or write #'princ) test test-not))
  database)

(defun wm-resources (database window &key key test test-not)
  ;; Takes the resources associated with the RESOURCE_MANAGER property
  ;; of WINDOW (if any) and merges them with DATABASE.
  ;; KEY is a function used for converting value-strings, the default is
  ;; identity.  TEST and TEST-NOT are predicates used for filtering
  ;; which resources to include in the database.  They are called with
  ;; the name and results of the KEY function.
  (declare (type resource-database database)
	   (type window window)
	   (type (or null (function (string) t)) key)
	   (type (or null (function (list t) generalized-boolean))
                 test test-not))
  (declare (clx-values resource-database))
  (let ((string (get-property window :RESOURCE_MANAGER :type :STRING
			      :result-type 'string
			      :transform #'xlib::card8->char)))
    (when string
      (with-input-from-string (stream string)
	(read-resources database stream
			:key key :test test :test-not test-not)))))

(defun set-wm-resources (database window &key write test test-not)
  ;; Sets the resources associated with the RESOURCE_MANAGER property
  ;; of WINDOW.
  ;; WRITE is a function used for writing values, the default is #'princ
  ;; TEST and TEST-NOT are predicates used for filtering which resources
  ;; to include in the database.  They are called with the name and value.
  (declare (type resource-database database)
	   (type window window)
	   (type (or null (function (string stream) t)) write)
	   (type (or null (function (list t) generalized-boolean))
                 test test-not))
  (xlib::set-string-property
    window :RESOURCE_MANAGER
    (with-output-to-string (stream)
      (write-resources database stream :write write
		       :test test :test-not test-not))))

(defun root-resources (screen &key database key test test-not)
  "Returns a resource database containing the contents of the root window
   RESOURCE_MANAGER property for the given SCREEN. If SCREEN is a display,
   then its default screen is used. If an existing DATABASE is given, then
   resource values are merged with the DATABASE and the modified DATABASE is
   returned.

   TEST and TEST-NOT are predicates for selecting which resources are
   read.  Arguments are a resource name list and a resource value. The KEY
   function, if given, is called to convert a resource value string to the
   value given to TEST or TEST-NOT."

  (declare (type (or screen display) screen)
	   (type (or null resource-database) database)
	   (type (or null (function (string) t)) key)
	   (type (or null (function (list t) generalized-boolean)) test test-not)
	   (clx-values resource-database))
  (let* ((screen (if (type? screen 'display)
		     (display-default-screen screen)
		   screen))
	 (window (screen-root screen))
	 (database (or database (make-resource-database))))
    (wm-resources database window :key key :test test :test-not test-not)
    database))

(defun set-root-resources (screen &key test test-not (write #'princ) database)
  "Changes the contents of the root window RESOURCE_MANAGER property for the
   given SCREEN. If SCREEN is a display, then its default screen is used. 

   TEST and TEST-NOT are predicates for selecting which resources from the
   DATABASE are written.  Arguments are a resource name list and a resource
   value.  The WRITE function is used to convert a resource value into a
   string stored in the property."

  (declare (type (or screen display) screen)
	(type (or null resource-database) database)
	(type (or null (function (list t) generalized-boolean)) test test-not)
	(type (or null (function (string stream) t)) write)
	(clx-values resource-database))
  (let* ((screen (if (type? screen 'display)
		     (display-default-screen screen)
		   screen))
	 (window (screen-root screen)))
    (set-wm-resources database window
		      :write write :test test :test-not test-not)
    database))

(defsetf root-resources (screen &key test test-not (write #'princ))(database)
  `(set-root-resources
    ,screen :test ,test :test-not ,test-not :write ,write :database ,database))

(defun initialize-resource-database (display)
  ;; This function is (supposed to be) equivalent to the Xlib initialization
  ;; code.
  (declare (type display display))
  (let ((rdb (make-resource-database))
	(rootwin (screen-root (car (display-roots display)))))
    ;; First read the server defaults if present, otherwise from the default
    ;; resource file
    (if (get-property rootwin :RESOURCE_MANAGER)
	(xlib:wm-resources rdb rootwin)
      (let ((path (default-resources-pathname)))
	(when (and path (probe-file path))
	  (read-resources rdb path))))
    ;; Next read from the resources file 
    (let ((path (resources-pathname)))
      (when (and path (probe-file path))
	(read-resources rdb path)))
    (setf (display-xdefaults display) rdb)))
