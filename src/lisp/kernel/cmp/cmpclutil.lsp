(in-package :cmp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utilities stolen and modified from Robert Strandh's Cleavir code
;;;

;;; Given a list of items, return which is like the one given as
;;; argument, except that each item has been wrapped in a (singleton)
;;; list.

(defun listify (items)
  (mapcar #'list items))

;;; Take a single declaration specifier and return a list of
;;; canonicalized declaration specifiers.
(defun canonicalize-declaration-specifier (declaration-specifier)
  (cond ((member (car declaration-specifier)
		 '(declaration dynamic-extent ftype ignore ignorable
		   inline notinline special))
         (mapcar (lambda (entity)
                   `(,(car declaration-specifier) ,entity))
                 (cdr declaration-specifier))
	 #+(or)(loop for entity in (cdr declaration-specifier)
                  collect `(,(car declaration-specifier) ,entity)))
	((eq (car declaration-specifier) 'optimize)
         (mapcar (lambda (entity)
                   (if (symbolp entity)
                       `(optimize (,entity 3))
                       `(optimize ,entity)))
                 (cdr declaration-specifier))
	 #+(or)(loop for entity in (cdr declaration-specifier)
                  collect (if (symbolp entity)
                              `(optimize (,entity 3))
                              `(optimize ,entity))))
	((eq (car declaration-specifier) 'type)
         (mapcar (lambda (entity)
                   `(type ,(cadr declaration-specifier) ,entity))
                 (cddr declaration-specifier))
	 #+(or)(loop for entity in (cddr declaration-specifier)
                  collect `(type ,(cadr declaration-specifier) ,entity)))
	(t
         (mapcar (lambda (entity)
                   `(type ,(car declaration-specifier) ,entity))
                 (cdr declaration-specifier))
	 #+(or)(loop for entity in (cdr declaration-specifier)
                  collect `(type ,(car declaration-specifier) ,entity)))))

(defun canonicalize-declaration-specifiers (declaration-specifiers)
  (unless (proper-list-p declaration-specifiers)
    (error "declaration specifiers must be a proper list"))
  (reduce #'append (mapcar #'canonicalize-declaration-specifier
			   declaration-specifiers)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Separate an ordinary body such as a let or let* body that may
;;; contain declarations (but no documentation) into the declarations
;;; and the executable forms.
;;;
;;; If there are declarations after the first executable form (which
;;; is a syntax error), then those declarations will be considered
;;; part of the executable forms.

(defun separate-ordinary-body (body)
  (unless (proper-list-p body)
    (error "Ordinary body must be proper list: ~a" body))
  (let ((pos (position-if-not (lambda (item)
				(and (consp item)
				     (eq (car item) 'declare)))
			      body)))
    (if (null pos)
	(values body '())
	(values (subseq body 0 pos) (subseq body pos)))))


;;; Separate a list of canonicalized declaration specifiers into two
;;; disjoint sets, returned as two values.  The first set contains All
;;; the declerations specifiers that concern an ordinary variable
;;; named NAME, and the second set the remaining declaration specifiers.
(defun separate-declarations (canonicalized-declaration-specifiers name)
  (let (first-rev second-rev)
    (do* ((spec-cur canonicalized-declaration-specifiers (cdr spec-cur))
          (spec (car spec-cur) (car spec-cur)))
         ((null (cdr spec-cur)) (values (nreverse first-rev) (nreverse second-rev)))
      (if (or (and (eq (first spec) 'ignore)
                   (eq (second spec) name))
              (and (eq (first spec) 'ignorable)
                   (eq (second spec) name))
              (and (eq (first spec) 'dynamic-extent)
                   (eq (second spec) name))
              (and (eq (first spec) 'special)
                   (eq (second spec) name))
              (and (eq (first spec) 'type)
                   (eq (third spec) name)))
          (push spec first-rev)
          (push spec second-rev))))
  #+(or)(loop for spec in canonicalized-declaration-specifiers
           if (or (and (eq (first spec) 'ignore)
                       (eq (second spec) name))
                  (and (eq (first spec) 'ignorable)
                       (eq (second spec) name))
                  (and (eq (first spec) 'dynamic-extent)
                       (eq (second spec) name))
                  (and (eq (first spec) 'special)
                       (eq (second spec) name))
                  (and (eq (first spec) 'type)
                       (eq (third spec) name)))
           collect spec into first
           else
           collect spec into second
           finally (return (values first second))))

;;; This function takes two arguments.  The first argument, VARIABLES,
;;; is a list of items, where each item is a non-empty list of symbols
;;; that are bound in a single binding.  The second items
;;; CANONICAL-DSPECS, is a list of canonicalized declaration
;;; specifiers.  This function returns a two values.  The first return
;;; value is a list with the same length as VARIABLES.  Each element
;;; in that list contains the elements in CANONICAL-DSPECS that apply
;;; to the corresponding element in VARIABLES.  The second return
;;; value is a list of the remaining declaration specifiers in
;;; CANONICAL-DSPECS i.e. the ones that do not apply to any element in
;;; VARIABLES.  A particular symbol S can not appear twice in an item
;;; of VARIABLES, but it can appear in different items.  In that case,
;;; the declaration specifiers that apply to that symbol will be
;;; associated with the last item in the list of VARIABLES.
(defun itemize-declaration-specifiers (variables canonical-dspecs)
  (if (null variables)
      (values '() canonical-dspecs)
      (multiple-value-bind (itemized-dspecs remaining-dspecs)
	  (itemize-declaration-specifiers (cdr variables) canonical-dspecs)
	(let ((item-specific-dspecs '()))
          (mapcar (lambda (var)
                    (multiple-value-bind (is-dspecs r-dspecs)
                        (separate-declarations remaining-dspecs var)
                      (setf item-specific-dspecs
                            (append is-dspecs item-specific-dspecs))
                      (setf remaining-dspecs r-dspecs)))
                  (first variables))
	  #+(or)(loop for var in (first variables)
                   do (multiple-value-bind (is-dspecs r-dspecs)
                          (separate-declarations remaining-dspecs var)
                        (setf item-specific-dspecs
                              (append is-dspecs item-specific-dspecs))
                        (setf remaining-dspecs r-dspecs)))
	  (values (cons item-specific-dspecs itemized-dspecs)
		  remaining-dspecs)))))
