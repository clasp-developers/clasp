;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 2009, Juan Jose Garcia-Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "C-BACKEND")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; C/C++ BACKEND
;;;

(defvar *c-opened-blocks* 0)

(defun c2driver (forms)
  (let ((*c-opened-blocks* 0))
    (loop for f in forms
       do (c2expr f)
       finally (when (plusp *c-opened-blocks*)
                 (error "Stray opened C blocks")))))
         
(defun c2expr (form)
  (cond ((consp form)
         (loop for f in form
            do (c2expr f)))
        ((tag-p form)
         (pprint-c1form form *dump-output*)
         (when (plusp (tag-ref form))
           (let ((label (tag-label form)))
             (unless label
               (error "No label for tag ~A" form))
             (wt-label label))))
        ((c1form-p form)
         (pprint-c1form form *dump-output*)
         (let* ((*file* (c1form-file form))
                (*file-position* (c1form-file form))
                (*current-form* (c1form-form form))
                (*current-toplevel-form* (c1form-toplevel-form form))
                (*current-c2form* form)
                (*cmp-env* (c1form-env form))
                (name (c1form-name form))
                (args (c1form-args form))
                (dispatch (gethash name +c2-dispatch-table+)))
           (unless dispatch
             (error "Unknown C1 form ~A" form))
           (apply dispatch args)))
        (t
         (error "In C2EXPR, invalid C1 form ~A" form))))

;;;
;;; C-BLOCKS
;;;

(defun open-c-block (&optional no-new-line)
  (unless no-new-line
    (if (zerop *c-opened-blocks*)
        (wt-nl1)
        (wt-nl)))
  (incf *c-opened-blocks*)
  (wt "{"))

(defun close-all-c-blocks (&optional (final-value 0))
  (let ((n (- *c-opened-blocks* final-value)))
    (when (plusp n)
      (if (zerop final-value)
          (wt-nl1)
          (wt-nl))
      (dotimes (i n)
        (wt "}"))
      (setf *c-opened-blocks* final-value))))

(defun close-c-block ()
  (unless (plusp *c-opened-blocks*)
    (error "Internal error: number of C/C++ blocks does not match expected value"))
  (wt-nl) (wt "}") (decf *c-opened-blocks*))

(defmacro with-c-block (&body code)
  `(unwind-protect
        (progn
          (open-c-block)
          ,@code)
     (close-c-block)))

;;;
;;; VARIABLE BINDINGS
;;;

(defun c2bind (temps)
  (loop with block-p = nil
     with new-env = t
     with closed-overs = '()
     for v in temps
     do (case (var-kind v)
          ((REPLACED DISCARDED))
          ((SPECIAL GLOBAL) (baboon))
          ((LEXICAL)
           (push v closed-overs))
          ((CLOSURE)
           (push v closed-overs)
           (when new-env
             (let ((env-lvl *env-lvl*))
               (format *dump-output* "~&;;; Increasing environment depth to ~D"
                       (1+ env-lvl))
               (unless block-p (open-c-block) (setf block-p t))
               (wt *volatile* "cl_object env" (incf *env-lvl*) " = env" env-lvl ";"))
             (setf new-env nil)))
          (t
           (setf (var-loc v) (next-lcl))
           (wt-nl)
           (unless block-p (open-c-block :no-newline) (setf block-p t))
           (wt *volatile* (rep-type-name (var-kind v)) " " v ";")
           (let ((name (var-name v))) (when name (wt-comment (var-name v))))))
     finally (loop for v in closed-overs do (bind NIL v))))

(defun c2bind-special (var value-loc)
  (bds-bind value-loc var))

(defun c2progv-op (destination vars-loc values-loc)
  (wt-nl destination "=ecl_progv(cl_env_copy," vars-loc "," values-loc ");"))

(defun c2progv-exit-op (ndx-loc)
  (wt-nl "ecl_bds_unwind(cl_env_copy," ndx-loc ");"))

(defun c2unbind-specials (nspecials)
  (case nspecials
    (0)
    (1 (wt-nl "ecl_bds_unwind1(cl_env_copy);"))
    (t (wt-nl "ecl_bds_unwind(cl_env_copy," nspecials ");"))))

(defun c2unbind (temps &optional (close-block t))
  (loop with nspecials = 0
     with closure = 0
     with block-p = nil
     for v in temps
     for kind = (var-kind v)
     do (case kind
          (CLOSURE (setf block-p t) (incf closure))
          ((SPECIAL GLOBAL) (incf nspecials))
          ((REPLACED DISCARDED LEXICAL))
          (otherwise (setf block-p t)))
     finally (progn
	       (c2unbind-specials nspecials)
	       (unless (zerop closure)
                 (wt-nl "/* End of lifetime of env" *env-lvl* "*/")
		 (decf *env-lvl*)
                 (format *dump-output* "~&;;; Decreasing environment depth to ~D"
                         *env-lvl*)
                 (decf *env* closure))
               (when block-p (close-c-block)))))

;;;
;;; ASSIGNMENTS
;;;

(defun c2set (loc value)
  (cond ((eq loc value)
         (cmpnote "Dummy SET statement ~A <- ~A" loc value)
         (unless (equal loc *destination*)
           (format *dump-output* "~&;;; In dummy SET, destination ~A /= loc ~A"
                   *destination* loc)))
        (t
         (set-loc value loc))))

(defun c2set-mv (locations min-args max-args)
  (let* ((extras (nthcdr max-args locations))
         (locations (ldiff locations extras)))
    (loop for v in extras
       do (bind nil v))
    (with-c-block
      (if (plusp min-args)
          (wt-nl "int _nvalues = cl_env_copy->nvalues - " min-args ";")
          (wt-nl "int _nvalues = cl_env_copy->nvalues;"))
      (loop for i from 0 below min-args
         for v = (pop locations)
         do (set-loc `(VALUE ,i) v))
      (loop with last-label = (next-label)
         with labels = '()
         for v in locations
         for i from min-args
         for l = (next-label)
         do (progn
              (push l labels)
              (wt-nl "if (_nvalues-- <= 0) ") (wt-go l)
              (set-loc `(VALUE ,i) v))
         finally (progn
                   (wt-nl) (wt-go last-label)
                   (loop for l in (nreverse labels)
                      for v in (reverse locations)
                      do (wt-label l)
                      do (set-loc nil v))
                   (wt-label last-label))))))

(defun c2values-op (locations)
  (loop for i from 0
     for v in locations
     do (wt-nl "cl_env_copy->values[" i "]=" v ";")
     finally (wt-nl "cl_env_copy->nvalues=" i ";")))

;;;
;;; FUNCTION ARGUMENTS
;;;

(defun c2bind-requireds (var-loc-pairs)
  (loop with new-env = t
     with block-p = nil
     for (v . required-loc) in var-loc-pairs
     ;; This is a hack: the C backend imposes that the arguments have
     ;; always representation type :OBJECT
     do (when (member (var-kind v) '(:DOUBLE :CHAR :FIXUNM :FLOAT))
          (setf (var-kind v) :OBJECT))
     ;; Here we perform the actual binding.
     do (case (var-kind v)
          (CLOSURE
           (when new-env
             (let ((env-lvl *env-lvl*))
               (format *dump-output* "~&;;; Increasing environment depth to ~D"
                       (1+ env-lvl))
               (unless block-p (open-c-block) (setf block-p t))
               (wt *volatile* "cl_object env" (incf *env-lvl*)
                   " = env" env-lvl ";")
               (setf new-env nil))))
          ((SPECIAL GLOBAL LEXICAL REPLACED DISCARDED))
          (t
           (unless block-p (open-c-block) (setf block-p t)))))
  (loop for (v . required-loc) in var-loc-pairs
     do (bind required-loc v)))

(defconstant +simple-va-args+ (make-symbol "args"))
(defconstant +cl-va-args+ (make-symbol "cl_args"))
(defconstant +nargs-var+ (make-symbol "narg"))

(defun simple-varargs-loc-p (var)
  (string= (var-name var) +simple-va-args+))

(defun c2varargs-bind-op (nargs-loc varargs-loc minargs maxargs nkeywords check)
  (open-c-block)
  (wt-comment "Arguments parsing - begin")
  (when (plusp nkeywords)
    (wt-nl "cl_object keyvars[" (* 2 nkeywords) "];"))
  (if (simple-varargs-loc-p varargs-loc)
      (progn
        (wt-nl "va_list args;")
        (wt-comment "Remaining arguments list")
        (wt-nl "va_start(args,__ecl_last_arg);"))
      (progn
        (wt-nl "cl_va_list cl_args;")
        (wt-comment "Optional arguments list")
        (wt-nl "cl_va_start(cl_args,__ecl_last_arg,narg," minargs ");")))
  (when check
    (when (plusp minargs)
      (wt-nl "if (narg<" minargs ") FEwrong_num_arguments_anonym();"))
    (unless (>= maxargs call-arguments-limit)
      (wt-nl "if (narg>" maxargs ") FEwrong_num_arguments_anonym();")))
  (when (plusp minargs)
    (wt-nl "narg -= " minargs ";")))

(defun c2varargs-pop-op (destination nargs-loc varargs-loc)
  (set-loc (if (simple-varargs-loc-p varargs-loc) 'VA-ARG 'CL-VA-ARG)
           destination))

(defun c2varargs-rest-op (dest-loc nargs-loc varargs-loc nkeys
                          keywords-loc allow-other-keys)
  (if (not (or keywords-loc allow-other-keys))
      ;; Simple case: we do not need to parse keyword arguments, just
      ;; collect all &rest
      (set-loc (if (simple-varargs-loc-p varargs-loc)
                   '(c-inline (:object) "cl_grab_rest_args(args)" () t nil)
                   '(c-inline (:object) "cl_grab_rest_args(cl_args)" () t nil))
               dest-loc)
      ;; More complicated case: we may need to parse keyword arguments,
      ;; collect all remaining arguments in &rest and check or not for
      ;; the presence of unknown keywords.
      (progn
        (if keywords-loc
            (wt-nl "cl_parse_key(cl_args," nkeys ",&(" keywords-loc "),keyvars")
            (wt-nl "cl_parse_key(cl_args,0,NULL,NULL"))
        (if (and dest-loc (not (eq dest-loc 'TRASH)))
            (wt ",(cl_object*)&" dest-loc)
            (wt ",NULL"))
        (wt (if allow-other-keys ",TRUE);" ",FALSE);")))))

(defun c2varargs-unbind-op (nargs-loc varargs-loc minargs maxargs nkeywords)
  (when (simple-varargs-loc-p varargs-loc)
    (wt-nl "va_end(args);"))
  (close-c-block)
  (wt-comment "Arguments parsing - end"))

;;;
;;; JUMP FRAMES
;;;

(defun c2frame-set (var no-label)
  (if (eq var 'UNWIND-PROTECT)
      (wt-nl "if (ecl_frs_push(cl_env_copy,ECL_PROTECT_TAG)==0) ")
      (wt-nl "if (ecl_frs_push(cl_env_copy," var ")==0) "))
  (wt-go (tag-label no-label)))

(defun c2frame-pop (&optional var)
  (unless (and var (var-p var) (zerop (var-ref var)))
    (wt-nl "ecl_frs_pop(cl_env_copy);")))

(defun c2frame-save-next (var)
  (bind "(cl_object)(cl_env_copy->nlj_fr)" var))

(defun c2frame-jmp-next (var)
  (wt-nl "ecl_unwind(cl_env_copy," var ");"))

(defun c2frame-id (var)
  (set-loc '(VV "ECL_NEW_FRAME_ID(cl_env_copy)") var))

;;;
;;; STACK FRAMES
;;;

(defun c2stack-frame-open (var)
  ;; STACK-FRAME-OPEN always follows a BIND form and hence it needs
  ;; no opening / closing braces.
  (wt "struct ecl_stack_frame _ecl_inner_frame_aux;")
  (wt-nl *volatile* "cl_object _ecl_inner_frame = ecl_stack_frame_open(cl_env_copy,(cl_object)&_ecl_inner_frame_aux,0);")
  (bind "_ecl_inner_frame" var))

(defun c2stack-frame-push (frame-var value-loc)
  (setf value-loc (coerce-one-location value-loc :object))
  (wt-nl "ecl_stack_frame_push(" frame-var "," value-loc ");"))

(defun c2stack-frame-push-values (frame-var)
  (wt-nl "ecl_stack_frame_push_values(" frame-var ");"))

(defun c2stack-frame-pop-values (frame-var dest)
  (wt-nl "cl_env_copy->values[0]=ecl_stack_frame_pop_values(" frame-var ");")
  (unless (eq dest 'trash)
    (set-loc 'VALUES dest)))

(defun c2stack-frame-apply (frame-var function-loc)
  (wt-nl "cl_env_copy->values[0]=ecl_apply_from_stack_frame(" frame-var
         "," function-loc ");"))

(defun c2stack-frame-close (frame-var)
  (wt-nl "ecl_stack_frame_close(" frame-var ");"))

;;;
;;; LOCAL AND NONLOCAL CONTROL TRANSFER
;;;

(defun c2jmp (tag)
  (wt-nl) (wt-go (tag-label tag)))

(defun set-loc-jmp-true (loc tag)
  (wt-nl "if (" (coerce-one-location loc :bool) ") ")
  (wt-go (tag-label tag)))

(defun set-loc-jmp-false (loc tag)
  (wt-nl "if (!(" (coerce-one-location loc :bool) ")) ")
  (wt-go (tag-label tag)))

(defconstant +integer-representation-types+
  '#.(loop for records on +representation-types+ by #'cddr
        for (name (type c-type) &rest) = records
        when (subtypep type 'integer)
        collect name))

(defun set-loc-jmp-zero (loc tag)
  (assert (member (loc-representation-type loc) +integer-representation-types+
                  :test #'eq))
  (wt-nl "if (!(" loc ")) ")
  (wt-go (tag-label tag)))

(defun set-loc-jmp-nonzero (loc tag)
  (assert (member (loc-representation-type loc) +integer-representation-types+
                  :test #'eq))
  (wt-nl "if ((" loc ")) ")
  (wt-go (tag-label tag)))

(defun c2return-from-op (var name)
  (wt-nl "cl_return_from(" var "," (add-symbol name) ");"))

(defun c2throw-op (tag)
  (wt-nl "cl_throw(" (coerce-one-location tag :object) ");"))

(defun c2go-op (tag)
  (let ((var (tag-var tag)))
    (wt-nl "cl_go(" var ",MAKE_FIXNUM(" (tag-index tag) "));")))

;;;
;;; FUNCTION CALLS, CLOSURES AND THE LIKE
;;;

(defun c2do-flet/labels (local-funs)
  ;; FIXME! We change the order for compatibility to make "diff"
  ;; with previous sources easier
  (mapc #'c::new-local (reverse local-funs)))

(defun c2funcall-op (destination args)
  (let* ((loc (pop args))
	 (form-type (location-primary-type loc))
         (function-p (and (subtypep form-type 'function)
                          (policy-assume-right-type))))
    (set-loc (call-unknown-global-loc nil loc args function-p)
             destination)))

(defun c2call-local (destination fun args)
  (set-loc (call-normal-loc destination fun args) destination))

(defun c2call-global (destination fname args &optional (return-type T))
  (let ((fun (find fname *global-funs* :key #'fun-name :test #'same-fname-p)))
    (set-loc (call-global-loc destination fname fun args return-type
                              (location-type destination))
             destination)))

(defun call-global-loc (destination fname fun args return-type expected-type)
  ;; Check whether it is a global function that we cannot call directly.
  (when (and (or (null fun) (fun-global fun)) (not (inline-possible fname)))
    (return-from call-global-loc
      (call-unknown-global-loc fname nil args)))

  ;; Open-codable function.
  (let* ((arg-types (mapcar #'location-primary-type args))
         (ii (inline-function destination fname arg-types
                              (type-and return-type expected-type))))
    (when ii
      (return-from call-global-loc (apply-inline-info ii args))))

  ;; Call to a function defined in the same file. Direct calls are
  ;; only emitted for low or neutral values of DEBUG >= 2.
  (when (and (<= (cmp-env-optimization 'debug) 1)
             (or (fun-p fun)
                 (and (null fun)
                      (setf fun (find fname *global-funs* :test #'same-fname-p
				      :key #'fun-name)))))
    (return-from call-global-loc (call-normal-loc fname fun args)))

  ;; Call to a global (SETF ...) function
  (when (not (symbolp fname))
    (return-from call-global-loc (call-unknown-global-loc fname nil args)))

  ;; Call to a function whose C language function name is known,
  ;; either because it has been proclaimed so, or because it belongs
  ;; to the runtime.
  (when (and (<= (cmp-env-optimization 'debug) 1)
             (setf fd (sys:get-sysprop fname 'Lfun)))
    (multiple-value-bind (minarg maxarg) (get-proclaimed-narg fname)
      (return-from call-global-loc
        (call-exported-function-loc
         fname args fd minarg maxarg
         (member fname c-data::*in-all-symbols-functions*)))))

  (multiple-value-bind (found fd minarg maxarg)
      (si::mangle-name fname t)
    (when found
      (return-from call-global-loc
        (call-exported-function-loc fname args fd minarg maxarg t))))

  (call-unknown-global-loc fname nil args))

(defun coerce-one-location (location rep-type)
  (if (eq rep-type (loc-representation-type location))
      location
      `(COERCE-LOC ,rep-type ,location)))

(defun coerce-locations (locations &optional types args-to-be-saved)
  (loop for i from 0
     for loc in locations
     for type = (if types (pop types) :object)
     for rep-type = (lisp-type->rep-type type)
     when (and args-to-be-saved (member i args-to-be-saved)
               (not (var-p loc)))
     do (cmpnote "Ignoring '~{@~A;~}' in a c-inline form" args-to-be-saved)
     collect (coerce-one-location loc rep-type)))

(defun call-normal-loc (fname fun args)
  `(CALL-NORMAL ,fun ,(coerce-locations args)))

(defun call-exported-function-loc (fname args fun-c-name minarg maxarg in-core)
  (unless in-core
    ;; We only write declarations for functions which are not in lisp_external.h
    (multiple-value-bind (val declared)
	(gethash fun-c-name *compiler-declared-globals*)
      (unless declared
	(if (= maxarg minarg)
	    (progn
	      (wt-nl-h "extern cl_object " fun-c-name "(")
	      (dotimes (i maxarg)
		(when (> i 0) (wt-h1 ","))
		(wt-h1 "cl_object"))
	      (wt-h1 ");"))
	    (progn
	      (wt-nl-h "#ifdef __cplusplus")
	      (wt-nl-h "extern cl_object " fun-c-name "(...);")
	      (wt-nl-h "#else")
	      (wt-nl-h "extern cl_object " fun-c-name "();")
	      (wt-nl-h "#endif")))
	(setf (gethash fun-c-name *compiler-declared-globals*) 1))))
  (let ((fun (make-fun :name fname :global t :cfun fun-c-name :lambda 'NIL
		       :minarg minarg :maxarg maxarg)))
    (call-normal-loc fname fun args)))

(defun call-unknown-global-loc (fname loc args &optional function-p)
  (unless loc
    (if (and (symbolp fname)
             (not (eql (symbol-package fname) 
                       (find-package "CL"))))
        (setf loc (add-symbol fname)
              function-p nil)
        (setf loc (list 'FDEFINITION fname)
              function-p t)))
  `(CALL-INDIRECT ,loc ,(coerce-locations args) ,fname ,function-p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FUNCTION CALL LOCATIONS
;;;

(defun wt-call-indirect (fun-loc args fname function-p)
  (let ((narg (length args)))
    (if function-p
        (wt "(cl_env_copy->function=" fun-loc ")->cfun.entry(" narg)
        (wt "ecl_function_dispatch(cl_env_copy," fun-loc ")(" narg))
    (dolist (arg args)
      (wt "," arg))
    (wt ")")
    (when fname (wt-comment fname))))

(defun wt-call-normal (fun args)
  (unless (fun-cfun fun)
    (baboon "Function without a C name: ~A" (fun-name fun)))
  (let* ((minarg (fun-minarg fun))
	 (maxarg (fun-maxarg fun))
	 (fun-c-name (fun-cfun fun))
	 (fun-lisp-name (fun-name fun))
	 (narg (length args))
	 (env nil))
    (case (fun-closure fun)
      (CLOSURE
       (setf env (environment-accessor fun)))
      (LEXICAL
       (let ((lex-lvl (fun-level fun)))
	 (dotimes (n lex-lvl)
	   (let* ((j (- lex-lvl n 1))
		  (x (lex-env-var-name j)))
	     (push x args))))))
    (unless (<= minarg narg maxarg)
      (cmperr "Wrong number of arguments for function ~S"
              (or fun-lisp-name 'ANONYMOUS)))
    (when (fun-narg-p fun)
      (push narg args))
    (wt-call fun-c-name args fun-lisp-name env)))

(defun wt-call (fun args &optional fname env)
  (if env
      (progn
        (pushnew :aux-closure (fun-code-gen-props *current-function*))
        (wt "(aux_closure.env=" env ",cl_env_copy->function=(cl_object)&aux_closure,")
        (wt-call fun args)
        (wt ")"))
      (progn
        (wt fun "(")
        (let ((comma ""))
          (dolist (arg args)
            (wt comma arg)
            (setf comma ",")))
        (wt ")")))
  (when fname (wt-comment fname)))

;;;
;;; DEBUG INFORMATION
;;;

(defun c2debug-env-open (fname)
  (open-c-block)
  (wt "struct ihs_frame _ecl_ihs;")
  (wt-comment "Debug info for:")
  (wt-nl "const cl_object _ecl_debug_env = Cnil;")
  (wt-comment fname)
  (wt-nl "ecl_ihs_push(cl_env_copy,&_ecl_ihs," (add-symbol fname) ",_ecl_debug_env);")
  (wt-nl))

(defun c2debug-env-close (fname)
  (wt-nl)
  (wt-nl "ecl_ihs_pop(cl_env_copy);")
  (wt-comment "Debug info removed")
  (close-c-block))

(defun c2debug-env-push-vars (variables)
  #-:msvc ;; FIXME! Problem with initialization of statically defined vectors
  (let* ((filtered-locations '())
         (filtered-codes '()))
    (open-c-block)
    (wt-comment "Debug bindings - register")
    ;; Filter out variables that we know how to store in the
    ;; debug information table. This excludes among other things
    ;; closures and special variables.
    (loop for var in variables
          for name = (let ((*package* (find-package "KEYWORD")))
                       (format nil "\"~S\"" (var-name var)))
          for code = (locative-type-from-var-kind (var-kind var))
          for loc = (var-loc var)
          when (and code (consp loc) (eq (first loc) 'LCL))
          do (progn
               (push (cons name code) filtered-codes)
               (push (second loc) filtered-locations)))
    ;; Generate two tables, a static one with information about the
    ;; variables, including name and type, and dynamic one, which is
    ;; a vector of pointer to the variables.
    (when filtered-codes
      (wt-nl "static const struct ecl_var_debug_info _ecl_descriptors[]={")
      (loop for (name . code) in filtered-codes
            for i from 0
            do (wt-nl (if (zerop i) "{" ",{") name "," code "}"))
      (wt "};")
      (wt-nl "const cl_index _ecl_debug_info_raw[]={")
      (wt-nl "(cl_index)(_ecl_debug_env),(cl_index)(_ecl_descriptors)")
      (loop for var-loc in filtered-locations
            do (wt ",(cl_index)(&" (lcl-name var-loc) ")"))
      (wt "};")
      (wt-nl "ecl_def_ct_vector(_ecl_debug_env,aet_index,_ecl_debug_info_raw,"
             (+ 2 (length filtered-locations))
             ",,);"))
    (when filtered-codes
      (wt-nl "ihs.lex_env=_ecl_debug_env;"))
    (wt-nl)))

(defun c2debug-env-pop-vars (variables close-block)
  #-:msvc
  (progn
    (wt-nl)
    (wt-comment "Debug bindings - remove")
    (wt-nl "ihs.lex_env=_ecl_debug_env->vector.self.t[0];")
    (when close-block (close-c-block))))

;;;
;;; FUNCTION PROLOGUE AND EPILOGUE
;;;

(defun c2emit-function-declaration (fun)
  (let* ((cfun (fun-cfun fun))
         (comma "")
         (narg-p (fun-narg-p fun)))
    (cond ((fun-exported fun)
           (wt-nl-h "ECL_DLLEXPORT cl_object " cfun "(")
           (wt-nl1 "cl_object " cfun "("))
          (t
           (wt-nl-h "static cl_object " cfun "(")
           (wt-nl1 "static cl_object " cfun "(")))
    (when narg-p
      (wt-h *volatile* "cl_narg")
      (wt *volatile* "cl_narg narg")
      (setf comma ", "))
    (when (eq (fun-closure fun) 'LEXICAL)
      (dotimes (n (fun-level fun))
        (wt-h comma *volatile* "cl_object  *")
        (wt comma *volatile* "cl_object *lex" n)
        (setf comma ", ")))
    (loop for lcl from 1 to (fun-minarg fun)
       do (progn
            (wt-h comma "cl_object " *volatile*)
            (wt comma "cl_object " *volatile*) (wt-lcl lcl)
            (setf comma ", ")))
    (when narg-p
      (wt-h ", ...")
      (wt ", ..."))
    (wt-h ");")
    (wt ")")))

(defun c2emit-local-variables (fun)
  ;; The following are macros containing the local variables that will
  ;; be needed. They are defined at the end.
  (let ((cname (fun-cfun fun)))
    (wt-nl "XTR_" cname))
  (wt-nl "const cl_env_ptr cl_env_copy = ecl_process_env();")
  (wt-nl "cl_object " *volatile* 
         (if (eq (fun-closure fun) 'CLOSURE)
             "env0 = cl_env_copy->function->cclosure.env;"
             "env0 = Cnil;"))
  (wt-nl "cl_object " *volatile* "value0;")
  (when (policy-check-stack-overflow)
    (wt-nl "ecl_cs_check(cl_env_copy,value0);")
    (wt-comment "C stack overflow?")))

(defun c2emit-closure-scan (fun)
  "Scans the environment looking for the locations of all closure variables we
actually use."
  (let ((clv-used (and (eq (fun-closure fun) 'CLOSURE)
                       (function-closure-variables fun))))
    (when clv-used
      (loop for v in clv-used
            do (format *dump-output*
                       "~&;;; Closure variable ~A loc ~S"
                       (var-name v) (var-loc v)))
      (wt-nl "/* Scanning closure data ... */")
      (loop with first = t
            with variables = (sort clv-used #'> :key #'var-loc)
            with v = (pop variables)
            with loc = (var-loc v)
            for n from (1- (fun-env fun)) downto 0
            while v
            do (progn
                 (when (> loc n)
                   (error "Inconsistent value of variable location ~A and fun-env for ~A"
                          (var-name a) (fun-name fun)))
                 (wt-nl "cl_object CLV" n)
                 (if first
                     (progn (wt "=env0;") (setf first nil))
                     (wt "=CDR(CLV" (1+ n) ");"))
                 (when (= n loc)
                   (wt-comment (var-name v))
                   (setf v (pop variables)
                         loc (and v (var-loc v))))))
      (wt-nl "/* ... closure scanning finished */"))))

(defun c2emit-last-arg-macro (fun)
  (when (fun-narg-p fun)
    (let ((nreq (fun-minarg fun)))
      (wt-nl1 "#define __ecl_last_arg "
              (cond ((plusp nreq)
                     (format nil "V~d" nreq))
                    ((eq (fun-closure fun) 'LEXICAL)
                     (format nil "lex~D" (1- (fun-level fun))))
                    (t "narg")))
      (wt-comment "Last argument before '...'"))))

(defun c2entry-function-prologue (fun &key shared-data)
  (wt-nl1 "#define flag V1")
  (open-c-block)
  (wt "cl_object *VVtemp;")
  (wt-comment "Entry point of ECL module / FASL")
  (when shared-data
    (wt-nl "Cblock=flag;")
    (wt-nl "VV = flag->cblock.data;"))
  (unless shared-data
    (wt-nl "if (!FIXNUMP(flag)){")
    (wt-comment "Creation of lisp data")
    (wt-nl "Cblock=flag;")
    (wt-nl1 "#ifndef ECL_DYNAMIC_VV")
    (wt-nl "flag->cblock.data = VV;")
    (wt-nl1 "#endif")
    (when *self-destructing-fasl*
      (wt-nl "flag->cblock.self_destruct=1;"))
    (wt-nl "flag->cblock.data_size = VM;")
    (wt-nl "flag->cblock.temp_data_size = VMtemp;")
    (wt-nl "flag->cblock.data_text = compiler_data_text;")
    (wt-nl "flag->cblock.data_text_size = compiler_data_text_size;")
    (wt-nl "flag->cblock.cfuns_size = compiler_cfuns_size;")
    (wt-nl "flag->cblock.cfuns = compiler_cfuns;")
    (when *compile-file-truename*
      (wt-nl "flag->cblock.source = make_constant_base_string(\""
             (namestring *compile-file-truename*) "\");"))
    (wt-nl "return;}")
    (wt-nl "#ifdef ECL_DYNAMIC_VV")
    (wt-nl "VV = Cblock->cblock.data;")
    (wt-nl "#endif")
    ;; With this we ensure creating a constant with the tag
    ;; and the initialization file
    (wt-nl "Cblock->cblock.data_text = \"" (c-tags:init-name-tag (fun-cfun fun)) "\";")
    )
  (when si::*compiler-constants*
    (wt-nl "{cl_object data = ecl_symbol_value("
           (nth-value 1 (si::mangle-name '*compiler-constants* nil))
           ");")
    (wt-nl "memcpy(VV, data->vector.self.t, VM*sizeof(cl_object));}"))
  (wt-nl "VVtemp = Cblock->cblock.temp_data;")
  (wt-comment "Here all lisp data has been created")
  (wt-nl)
  (wt-comment "It follows all toplevel forms"))

(defun c2function-prologue (fun)
  (wt-comment-nl (cond ((fun-global fun) "function definition for ~a")
                       ((eq (fun-closure fun) 'CLOSURE) "closure ~a")
                       (t "local function ~a"))
                 (or (fun-name fun) (fun-description fun) 'CLOSURE))
  (c2emit-function-declaration fun)
  (open-c-block :function)
  (format *dump-output* "~&;;; Environment depth ~A" *env-lvl*)
  (format *dump-output* "~&;;; Environment size ~A" *env*)
  (c2emit-local-variables fun)
  (c2emit-last-arg-macro fun)
  (c2emit-closure-scan fun)
  (when (eq (fun-name fun) +init-function-name+)
    (c2entry-function-prologue fun)))

(defun c2function-epilogue (fun)
  (let* ((name (fun-cfun fun))
         (closure (fun-closure fun)))
    ;; There should be no need to mark lex as volatile, since we
    ;; are going to pass pointers of this array around and the compiler
    ;; should definitely keep this in memory.
    (wt-nl-h "#define XTR_" name)
    (when (plusp *max-lex*)
      (wt-h " \\")
      (wt-nl-h " volatile cl_object lex" *level* "[" *max-lex* "];"))
    (when (member :aux-closure (fun-code-gen-props fun))
      (wt-h " \\")
      (wt-nl-h " struct ecl_cclosure aux_closure;"))
    ;; Close C blocks
    (when (fun-narg-p fun)
      (wt-nl1 "#undef __ecl_last_arg"))
    (unless (zerop *env-lvl*)
      (error "Wrong value of environment depth ~A" *env-lvl*))
    (unless (= *env* (fun-env fun))
      (error "Wrong value of environment size ~A" *env*))
    (close-all-c-blocks)))

;;;
;;; FSET FIXME! UNUSED!
;;;

(defun c2fset (fun fname macro pprint c1forms)
  (when (fun-no-entry fun)
    (wt-nl "(void)0; /* No entry created for "
	   (format nil "~A" (fun-name fun))
	   " */")
    ;; FIXME! Look at c2function!
    (new-local fun)
    (return-from c2fset))
  (when (fun-closure fun)
    (return-from c2fset (c2call-global destination 'SI:FSET c1forms)))
  (let ((*inline-blocks* 0)
	(loc (data-empty-loc)))
    (push (list loc fname fun) *global-cfuns-array*)
    ;; FIXME! Look at c2function!
    (new-local fun)
    (wt-nl (if macro "ecl_cmp_defmacro(" "ecl_cmp_defun(")
	   loc ");")
    (close-inline-blocks)))

