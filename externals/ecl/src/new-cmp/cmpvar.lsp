;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; CMPVAR  Variables.

(in-package "COMPILER")

(defun make-var (&rest args)
  (let ((var (apply #'%make-var args)))
    (unless (member (var-kind var) '(SPECIAL GLOBAL))
      (when *current-function*
	(push var (fun-local-vars *current-function*))))
    var))

;;; FIXME! VAR-REFERENCED-IN-FORM and VAR-CHANGED-IN-FORM are too
;;; pessimistic. One should check whether the functions reading/setting the
;;; variable are actually called from the given node.  The problem arises when
;;; we create a closure of a function, as in
;;;
;;;	(let* ((a 1) (b #'(lambda () (incf a)))) ...)
;;;
;;; To know whether A is changed or read, we would have to track where B is
;;; actually used.

(defun var-referenced-in-form (var form)
  (declare (type var var))
  (cond ((consp form)
         (loop for f in form thereis (var-referenced-in-form var f)))
        ((c1form-p form)
         (member form (var-read-nodes var) :test #'eq))
        (t
         nil)))

(defun var-referenced-in-form (var form)
  (declare (type var var))
  (cond ((eq (var-kind var) 'REPLACED)
         (let ((loc (var-loc var)))
           (when (var-p loc)
             (var-referenced-in-forms loc form))))
        ((global-var-p var) ; Too pessimistic?
         t)
        ((null (var-read-nodes var))
         nil)
        ((listp form)
         (loop for f in form thereis (var-referenced-in-form var f)))
        ((c1form-p form)
         (member form (var-read-nodes var) :test #'eq))
        (t
         nil)))

(defun var-changed-in-form (var form)
  (declare (type var var))
  (cond ((eq (var-kind var) 'REPLACED)
         (let ((loc (var-loc var)))
           (when (var-p loc)
             (var-referenced-in-forms loc form))))
        ((and (null (var-set-nodes var))
              (not (global-var-p var)))
         nil)
        ((listp form)
         (loop for f in form thereis (var-changed-in-form var f)))
        ((not (c1form-p form))
         nil)
        ((global-var-p var) ; Too pessimistic?
         (c1form-sp-change form))
        (t
         (member form (var-set-nodes var) :test #'eq))))

(defun add-to-read-nodes (var forms)
  (dolist (form forms)
    (incf (var-ref var))
    (push form (var-read-nodes var))
    (when *current-function*
      (unless (eq *current-function* (var-function var))
        (pushnew *current-function* (var-functions-reading var))
        (pushnew var (fun-referred-vars *current-function*)))))
  forms)

(defun add-to-set-nodes (var forms)
  (dolist (form forms)
    (incf (var-ref var))
    (push form (var-set-nodes var))
    ;;(push form (var-read-nodes var))
    (when *current-function*
      (unless (eq *current-function* (var-function var))
        (pushnew *current-function* (var-functions-setting var))
        (pushnew var (fun-referred-vars *current-function*)))))
  forms)

(defun add-to-set-nodes-of-var-list (var-list forms)
  (dolist (v var-list)
    (add-to-set-nodes v forms))
  forms)

(defun eliminate-from-read-nodes (var form)
  (when (var-p var)
    (setf (var-ref var) (1- (var-ref var))
          (var-read-nodes var) (delete form (var-read-nodes var)))))

(defun eliminate-from-set-nodes (var form)
  (when (var-p var)
    (setf (var-set-nodes var) (delete form (var-set-nodes var)))))

;;; A special binding creates a var object with the kind field SPECIAL,
;;; whereas a special declaration without binding creates a var object with
;;; the kind field GLOBAL.  Thus a reference to GLOBAL may need to make sure
;;; that the variable has a value.

;;;
;;; Check if a variable has been declared as a special variable with a global
;;; value.

(defun check-global (name)
  (member name *global-vars* :test #'eq :key #'var-name))

;;;
;;; Check if the symbol has a symbol macro
;;;
(defun chk-symbol-macrolet (form)
  (loop
   (when (not (symbolp form))
     (return form))
   (let ((new-form (macroexpand-1 form *cmp-env*)))
     (when (eq new-form form)
       (return form))
     (setf form new-form))))

(defun c1make-var (name specials ignores types)
  (cmpck (not (symbolp name)) "The variable ~s is not a symbol." name)
  (cmpck (constantp name) "The constant ~s is being bound." name)
  (let (type)
    (if (setq type (assoc name types))
	(setq type (cdr type))
	(setq type 'T))
    (cond ((or (member name specials)
	       (sys:specialp name)
               (check-global name))	;; added. Beppe 17 Aug 1987
           (unless type
	     (setf type (or (get-sysprop name 'CMP-TYPE) 'T)))
	   (c1make-global-variable name :kind 'SPECIAL :type type))
          (t
	   (make-var :name name :type type :loc 'OBJECT
		     :kind 'LEXICAL ; we rely on check-vref to fix it
		     :ref (if (member name ignores) -1 0))))))

(defun c1var (destination name)
  (let ((vref (c1vref name (eq destination 'TRASH))))
    (when vref
      (c1set-loc destination vref))))

(defun make-lcl-var (&key rep-type (type 'T))
  (unless rep-type
    (setq rep-type (if type (lisp-type->rep-type type) :object)))
  (unless type
    (setq type 'T))
  (make-var :kind rep-type :type type :loc (next-lcl)))

(defun make-temp-var (&optional (type 'T))
  (make-var :kind :object :type type :loc `(TEMP ,(next-temp))))

(defun c1vref (name &optional maybe-drop-ref)
  (multiple-value-bind (var ccb clb unw)
      (cmp-env-search-var name)
    (cond ((null var)
           (unless (and maybe-drop-ref (not (policy-global-var-checking)))
             (c1make-global-variable name :warn t
                                     :type (or (get-sysprop name 'CMP-TYPE) t))))
	  ((not (var-p var))
	   ;; symbol-macrolet
	   (baboon))
	  (t
           (when (and maybe-drop-ref
                      (not (and (global-var-p var)
                                (policy-global-var-checking))))
             (return-from c1vref nil))
           (when (minusp (var-ref var)) ; IGNORE.
             (cmpwarn-style "The ignored variable ~s is used." name)
             (setf (var-ref var) 0))
	   (when (eq (var-kind var) 'LEXICAL)
	     (cond (ccb (setf (var-ref-clb var) nil ; replace a previous 'CLB
			      (var-ref-ccb var) t
			      (var-kind var) 'CLOSURE
			      (var-loc var) 'OBJECT))
		   (clb (setf (var-ref-clb var) t
			      (var-loc var) 'OBJECT))))
	   var))))

(defun global-var-p (var)
  (and (var-p var)
       (member (var-kind var) '(SPECIAL GLOBAL) :test #'eq)))

(defun local-var-p (var)
  (and (var-p var)
       (let ((kind (var-kind var)))
         (unless (member kind '(LEXICAL CLOSURE SPECIAL GLOBAL REPLACED DISCARDED))
           kind))))

(defun temporal-var-p (var)
  ;; FIXME! Currently we have no other way of identifying temporal variables
  (var-read-only-p var))

;;; ----------------------------------------------------------------------

(defun c1make-global-variable (name &key (type t) (kind 'GLOBAL) (warn nil))
  (let ((var (find name *global-var-objects* :key #'var-name)))
    (unless var
      (setf var (make-var :name name :kind kind :type type :loc (add-symbol name))))
    (push var *global-var-objects*)
    (when warn
      (unless (or (sys:specialp name) (constantp name) (check-global name))
	(undefined-variable name)
	(push var *undefined-vars*)))
    var))

(defun c1declare-specials (globals)
  (mapc #'cmp-env-declare-special globals))

(defun si::register-global (name)
  (unless (check-global name)
    (push (c1make-global-variable name :kind 'GLOBAL
				  :type (or (get-sysprop name 'CMP-TYPE) 'T))
	  *global-vars*))
  (values))

(defun c1setq (destination args)
  (let ((l (length args)))
    (declare (fixnum l))
    (cmpck (oddp l) "SETQ requires an even number of arguments.")
    (cond ((zerop l) (c1nil destination))
	  ((= l 2) (c1setq1 destination (first args) (second args)))
	  (t
           (c1progn destination
                    (loop while args
                       collect `(SETQ ,(pop args) ,(pop args))))))))

(defun c1setq1 (destination name form)
  (cmpck (not (symbolp name)) "The variable ~s is not a symbol." name)
  (cmpck (constantp name) "The constant ~s is being assigned a value." name)
  (setq name (chk-symbol-macrolet name))
  (unless (symbolp name)
    (return-from c1setq1 (c1translate destination `(setf ,name ,form))))
  (c1with-saved-one-value (prefix postfix temp form)
    (let* ((name1 (c1vref name)))
      (nconc prefix
	     (c1set-loc name1 temp)
	     postfix
	     (unless (eq destination 'trash) (c1set-loc destination name1))))))

(defun unused-variable-p (var)
  "Is the value of the variable ever read?"
  (and (var-p var)
       (null (var-read-nodes var))
       (not (global-var-p var))))

(defun c1progv (destination args)
  (check-args-number 'PROGV args 2)
  (c1with-temps (ndx-prefix ndx-postfix bds-ndx)
    (let* ((variables (pop args))
	   (values (pop args)))
      (c1with-saved-values (prefix postfix temps (list variables values))
	(let* ((cleanup (c1progv-exit-op bds-ndx))
	       (*cmp-env* (cmp-env-register-cleanup cleanup
						    (cmp-env-copy *cmp-env*))))
	  (nconc ndx-prefix
		 prefix
		 (c1progv-op bds-ndx (first temps) (second temps))
		 (c1progn destination args)
                 (c1progv-exit-op bds-ndx)
		 postfix
		 ndx-postfix))))))

(defun c1psetq (destination args)
  (let* ((variables '())
         (values '())
         (use-psetf nil))
    (do ((l args))
        ((endp l))
      (declare (object l))
      (let ((var (pop l)))
        (cmpck (not (symbolp var))
               "The variable ~s is not a symbol." var)
        (cmpck (endp l)
               "No form was given for the value of ~s." var)
        (let* ((value (pop l))
               (expanded-var (chk-symbol-macrolet var)))
          (push value values)
          (push expanded-var variables)
          (if (symbolp expanded-var)
              (cmpck (constantp expanded-var)
                     "The constant ~s is being assigned a value."
                     expanded-var)
              (setq use-psetf t)))))
    (when use-psetf
      (setf args (mapcan #'list variables values))
      (return-from c1psetq (c1translate destination `(psetf ,@args))))
    (c1with-saved-values (prefix postfix temps values)
      (nconc prefix
             (loop for name in variables
                for vref = (c1vref name)
                for x in temps
                nconc (c1set-loc vref x))
             postfix
             (c1set-loc destination nil)))))
