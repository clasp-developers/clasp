(defun get-setf-expansion (form &optional env &aux f)
  "Args: (form)
Returns the 'five gangs' (see DEFINE-SETF-EXPANDER) for PLACE as five values.
Does not check if the third gang is a single-element list."
  (declare (check-arguments-type nil))
  ;; Note that macroexpansion of SETF arguments can only be done via
  ;; MACROEXPAND-1 [ANSI 5.1.2.7]
  (cond ((symbolp form)
	 (if (and (setq f (macroexpand-1 form env)) (not (equal f form)))
	     (progn
	       (print "stage 1")
	       (get-setf-expansion f env))
	     (let ((store (gensym "store-get-setf-expansion")))
	       (debug)
	       (print "stage 1.5")
	       (values nil nil (list store) `(setq ,form ,store) form))))
	((or (not (consp form)) (not (symbolp (car form))))
	 (error "Cannot get the setf-method of ~S." form))
	((setq f (get-sysprop (car form) 'SETF-METHOD))
	 (print (list "stage 2 f= " f))
	 (apply f env (cdr form)))
	((and (setq f (macroexpand-1 form env)) (not (equal f form)))
	 (print "stage 3")
	 (get-setf-expansion f env))
	(t
	 (print "stage 4")
	 (do-setf-method-expansion (car form) nil (cdr form)))))




(defmacro pop (&environment env place)
  "Syntax: (pop place)
Gets the cdr of the value stored in PLACE and makes it the new value of PLACE.
Returns the car of the old value in PLACE."
  (declare (notinline mapcar))
  (multiple-value-bind (vars vals stores store-form access-form)
      (get-setf-expansion place env)
    (print (list "vars= " vars " vals= " vals " stores= " stores " store-form= " store-form " access-form= " access-form))
    (let ((store-var (first stores))
	  (store-expansion (mapcar #'list (append vars stores) (append vals (list access-form)))))
      (print (list "pop store-expansion = " store-expansion))
      `(let* ,store-expansion
	 (declare (:read-only ,@vars)) ; Beppe
	 (prog1 (car ,store-var)
	   (setq ,store-var (cdr (truly-the list ,store-var)))
	   ,store-form)))))
