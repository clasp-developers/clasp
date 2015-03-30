;;;; reading circular data: the #= and ## readmacros

(in-package :core)


;;; objects already seen by CIRCLE-SUBST
(defvar *sharp-equal-circle-table*)
;;(declaim (type hash-table *sharp-equal-circle-table*))

;; This function is kind of like NSUBLIS, but checks for circularities and
;; substitutes in arrays and structures as well as lists. The first arg is an
;; alist of the things to be replaced assoc'd with the things to replace them.
#-use-sharp-equal-hash-tables
(defun circle-subst (old-new-alist tree)
  (cond ((not (typep tree '(or cons (array t) #| #+clos instance #+clos funcallable-instance |#)))
         (let ((entry (find tree old-new-alist :key #'second)))
           (if entry (third entry) tree)))
        ((null (gethash tree *sharp-equal-circle-table*))
         (setf (gethash tree *sharp-equal-circle-table*) t)
         (cond ((consp tree)
                (let ((a (circle-subst old-new-alist (car tree)))
                      (d (circle-subst old-new-alist (cdr tree))))
                  (unless (eq a (car tree))
                    (rplaca tree a))
                  (unless (eq d (cdr tree))
                    (rplacd tree d))))
               ((arrayp tree)
                  (do ((i 0 (1+ i)))
                      ((>= i (array-total-size tree)))
                    (let* ((old (row-major-aref tree i))
                           (new (circle-subst old-new-alist old)))
                      (unless (eq old new)
                        (setf (row-major-aref tree i) new))))

		)
#|	       #+clos ((typep tree 'instance)
		       (let* ((n-untagged (layout-n-untagged-slots (%instance-layout tree)))
			      (n-tagged (- (%instance-length tree) n-untagged)))
			 ;; N-TAGGED includes the layout as well (at index 0), which
			 ;; we don't grovel.
			 (do ((i 1 (1+ i)))
			     ((= i n-tagged))
			   (let* ((old (%instance-ref tree i))
				  (new (circle-subst old-new-alist old)))
			     (unless (eq old new)
			       (setf (%instance-ref tree i) new))))
			 (do ((i 0 (1+ i)))
			     ((= i n-untagged))
			   (let* ((old (%raw-instance-ref/word tree i))
				  (new (circle-subst old-new-alist old)))
			     (unless (= old new)
			       (setf (%raw-instance-ref/word tree i) new))))))
               #+clos ((typep tree 'funcallable-instance)
		       (do ((i 1 (1+ i))
			    (end (- (1+ (get-closure-length tree)) %funcallable-instance-info-offset)))
			   ((= i end))
			 (let* ((old (%funcallable-instance-info tree i))
				(new (circle-subst old-new-alist old)))
			   (unless (eq old new)
			     (setf (%funcallable-instance-info tree i) new)))))
|#
	       )
         tree)
        (t tree)))




;;; Sharp-equal works as follows. When a label is assigned (i.e. when
;;; #= is called) we GENSYM a symbol is which is used as an
;;; unforgeable tag. *SHARP-SHARP-ALIST* maps the integer tag to this
;;; gensym.
;;;
;;; When SHARP-SHARP encounters a reference to a label, it returns the
;;; symbol assoc'd with the label. Resolution of the reference is
;;; deferred until the read done by #= finishes. Any already resolved
;;; tags (in *SHARP-EQUAL-ALIST*) are simply returned.
;;;
;;; After reading of the #= form is completed, we add an entry to
;;; *SHARP-EQUAL-ALIST* that maps the gensym tag to the resolved
;;; object. Then for each entry in the *SHARP-SHARP-ALIST, the current
;;; object is searched and any uses of the gensysm token are replaced
;;; with the actual value.
#-use-sharp-equal-hash-tables
(defvar *sharp-sharp-alist* ())

#-use-sharp-equal-hash-tables
(defun sharp-equal (stream ignore label)
  (declare (ignore ignore))
  (when *read-suppress* (return-from sharp-equal (values)))
  (unless label
    (simple-reader-error stream "missing label for #=" label))
  (when (or (assoc label *sharp-sharp-alist*)
            (assoc label *sharp-equal-alist*))
    (simple-reader-error stream "multiply defined label: #~D=" label))
  (let* ((tag (gensym))
         (*sharp-sharp-alist* (acons label tag *sharp-sharp-alist*))
         (obj (read stream t nil t)))
    (when (eq obj tag)
      (simple-reader-error stream
                     "must tag something more than just #~D#"
                     label))
    (push (list label tag obj) *sharp-equal-alist*)
    (let ((*sharp-equal-circle-table* (make-hash-table :test 'eq :size 20)))
      (values (circle-subst *sharp-equal-alist* obj)))))

#-use-sharp-equal-hash-tables
(defun sharp-sharp (stream ignore label)
  (declare (ignore ignore))
  (when *read-suppress* (return-from sharp-sharp (values nil)))
  (unless label
    (simple-reader-error stream "missing label for ##" label))
  (let ((entry (assoc label *sharp-equal-alist*)))
    (if entry
        (values (third entry))
        (let (;; Has this label been defined previously? (Don't read
              ;; ANSI "2.4.8.15 Sharpsign Equal-Sign" and worry that
              ;; it requires you to implement forward references,
              ;; because forward references are disallowed in
              ;; "2.4.8.16 Sharpsign Sharpsign".)
              (pair (assoc label *sharp-sharp-alist*)))
          (unless pair
            (simple-reader-error stream
                                 "reference to undefined label #~D#"
                                 label))
          (values (cdr pair))))))





;;;  Modified from http://marc.info/?l=sbcl-devel&m=118219422006130
;;;
;;; The SHARP-EQUAL function introduces a label for an object, and the
;;; SHARP-SHARP function allows us to refer to this label.  This is slightly
;;; tricky because the object we are labelling can contain labels to itself in
;;; order to create circular structures, e.g., #1=(cons 'hello #1#).
;;;
;;; The SHARP-EQUAL function is called when we are reading a stream and
;;; encounter text of the form #<LABEL>=<OBJ>, where <LABEL> should be a number
;;; and <OBJ> ought to be readable as an object.  Our first step is to use
;;; gensym to create a new <TAG> for this label; we temporarily bind <LABEL> to
;;; <TAG> and then read in <OBJ> using our <TAG> as a temporary binding for
;;; <LABEL>.  Finally, we fix the <OBJ> by replacing any occurrences of <TAG>
;;; with a pointer to <OBJ> itself, creating the circular structures.
;;;
;;; We now do this with a couple of data structures.
;;;
;;; 1.  *SHARP-EQUAL-FINAL-TABLE* is a hash table where "finished" associations
;;;     are stored.  That is, it is a hash table from labels to objects, where
;;;     the objects have already been patched and are tag-free.
;;;
;;; 2.  *SHARP-EQUAL-TEMP-TABLE* is a hash table where "unfinished"
;;;     associations are stored.  That is, it is a hash table from labels to
;;;     tags.
;;;
;;; 3.  *SHARP-EQUAL-REPL-TABLE* is a hash table that associates tags with
;;;     their corrective pointers.  That is, this is the table we use to
;;;     "patch" the objects.

#+use-sharp-equal-hash-tables
(defun circle-subst (repl-table tree)
  (cond ((not (typep tree '(or cons (array t) #| #+clos instance #+clos funcallable-instance |#)))
	 (multiple-value-bind (value presentp)
	     (gethash tree repl-table)
	   (if presentp
	       value
	       tree)))
        ((null (gethash tree *sharp-equal-circle-table*))
         (setf (gethash tree *sharp-equal-circle-table*) t)
         (cond ((consp tree)
                (let ((a (circle-subst repl-table (car tree)))
                      (d (circle-subst repl-table (cdr tree))))
                  (unless (eq a (car tree))
                    (rplaca tree a))
                  (unless (eq d (cdr tree))
                    (rplacd tree d))))
               ((arrayp tree)
                  (do ((i 0 (1+ i)))
                      ((>= i (array-total-size tree)))
                    (let* ((old (row-major-aref tree i))
                           (new (circle-subst repl-table old)))
                      (unless (eq old new)
                        (setf (row-major-aref tree i) new))))

		)
#|	       #+clos ((typep tree 'instance)
		       (let* ((n-untagged (layout-n-untagged-slots (%instance-layout tree)))
			      (n-tagged (- (%instance-length tree) n-untagged)))
			 ;; N-TAGGED includes the layout as well (at index 0), which
			 ;; we don't grovel.
			 (do ((i 1 (1+ i)))
			     ((= i n-tagged))
			   (let* ((old (%instance-ref tree i))
				  (new (circle-subst repl-table old)))
			     (unless (eq old new)
			       (setf (%instance-ref tree i) new))))
			 (do ((i 0 (1+ i)))
			     ((= i n-untagged))
			   (let* ((old (%raw-instance-ref/word tree i))
				  (new (circle-subst repl-table old)))
			     (unless (= old new)
			       (setf (%raw-instance-ref/word tree i) new))))))
               #+clos ((typep tree 'funcallable-instance)
		       (do ((i 1 (1+ i))
			    (end (- (1+ (get-closure-length tree)) %funcallable-instance-info-offset)))
			   ((= i end))
			 (let* ((old (%funcallable-instance-info tree i))
				(new (circle-subst repl-table old)))
			   (unless (eq old new)
			     (setf (%funcallable-instance-info tree i) new)))))
|#
	       )
         tree)
        (t tree)))

#+use-sharp-equal-hash-tables
(defun sharp-equal (stream ignore label)
  (declare (ignore ignore))
  (when *read-suppress* (return-from sharp-equal (values)))
  (unless label
    (simple-reader-error stream "missing label for #=" label))
  (when (or (multiple-value-bind (value presentp)
		(gethash label *sharp-equal-final-table*)
	      (declare (ignore value))
	      presentp)
            (multiple-value-bind (value presentp)
		(gethash label *sharp-equal-temp-table*)
	      (declare (ignore value))
	      presentp))
    (simple-reader-error stream "multiply defined label: #~D=" label))
  (let ((tag (gensym)))
    (setf (gethash label *sharp-equal-temp-table*) tag)
    (let ((obj (read stream t nil t)))
      (when (eq obj tag)
	(%reader-error stream "must tag something more than just #~D#" label))
      (setf (gethash tag *sharp-equal-repl-table*) obj)
      (let ((*sharp-equal-circle-table* (make-hash-table :test 'eq :size 20)))
        (circle-subst *sharp-equal-repl-table* obj))
      (setf (gethash label *sharp-equal-final-table*) obj))))


#+use-sharp-equal-hash-tables
(defun sharp-sharp (stream ignore label)
  (declare (ignore ignore))
  (when *read-suppress* (return-from sharp-sharp (values nil)))
  (unless label
    (simple-reader-error stream "missing label for ##" label))
  ;; Don't read ANSI "2.4.8.15 Sharpsign Equal-Sign" and worry that it requires
  ;; you to implement forward references, because forward references are
  ;; disallowed in "2.4.8.16 Sharpsign Sharpsign".
  (multiple-value-bind
	(finalized-object successp)
      (gethash label *sharp-equal-final-table*)
    (if successp
	finalized-object
	(multiple-value-bind
	      (temporary-tag successp)
	    (gethash label *sharp-equal-temp-table*)
	  (if successp
	      temporary-tag
	      (%reader-error stream "reference to undefined label #~D#" label))))))












(defun sharpmacros-enhance ()
  (set-dispatch-macro-character #\# #\= #'sharp-equal)
  (set-dispatch-macro-character #\# #\# #'sharp-sharp)
)

(sharpmacros-enhance)






