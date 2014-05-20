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
