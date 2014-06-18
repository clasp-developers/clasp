;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  CMPTOP  --  Compiler top-level.

;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "COMPILER")

(defun t1expr (form)
  (let* ((*current-toplevel-form* nil)
         (*cmp-env* (if *cmp-env*
                        (cmp-env-copy *cmp-env*)
                        (cmp-env-root))))
    (push (t1expr* form) *top-level-forms*)))

(defvar *toplevel-forms-to-print*
  '(defun defmacro defvar defparameter defclass defmethod defgeneric))

(defun t1expr* (form &aux
                     (*current-toplevel-form* (list* form *current-toplevel-form*))
                     (*current-form* form)
                     (*first-error* t)
                     (*setjmps* 0))
  (when (consp form)
    (let ((fun (car form)) (args (cdr form)) fd)
      (when (member fun *toplevel-forms-to-print*)
	(print-current-form))
      (cond
	((consp fun) (t1ordinary form))
	((not (symbolp fun))
	 (cmperr "~s is illegal function." fun))
	((eq fun 'QUOTE)
	 (t1ordinary 'NIL))
	((setq fd (gethash fun *t1-dispatch-table*))
	 (funcall fd args))
	((gethash fun *c1-dispatch-table*)
	 (t1ordinary form))
	((and (setq fd (compiler-macro-function fun))
	      (inline-possible fun)
	      (let ((success nil))
		(multiple-value-setq (fd success)
		  (cmp-expand-macro fd form))
		success))
	 (push 'macroexpand *current-toplevel-form*)
	 (t1expr* fd))
	((setq fd (cmp-macro-function fun))
	 (push 'macroexpand *current-toplevel-form*)
	 (t1expr* (cmp-expand-macro fd form)))
	(t (t1ordinary form))
	))))

(defun t1/c1expr (form)
  (cond ((not *compile-toplevel*)
	 (c1expr form))
	((atom form)
	 (t1ordinary form))
	(t
         (t1expr* form))))

(defun t2expr (form)
  (when form
    (let* ((def (gethash (c1form-name form) *t2-dispatch-table*)))
      (if def
          (let ((*compile-file-truename* (c1form-file form))
                (*compile-file-position* (c1form-file-position form))
                (*current-toplevel-form* (c1form-form form))
                (*current-form* (c1form-form form))
                (*cmp-env* (c1form-env form)))
            (apply def form (c1form-args form)))
          (cmperr "Unhandled T2FORM found at the toplevel:~%~4I~A"
                  form)))))

(defvar *emitted-local-funs* nil)

#+nil
(defun emit-local-funs ()
  ;; Local functions and closure functions
  (do ()
      ((eq *local-funs* *emitted-local-funs*))
    (let ((to-be-emitted (ldiff *local-funs* *emitted-local-funs*)))
      (setf *emitted-local-funs* *local-funs*)
      (mapc #'t3local-fun (nreverse to-be-emitted)))))

(defun emit-local-funs ()
  ;; Local functions and closure functions
  (do ((*compile-time-too* nil)
       (*compile-toplevel* nil))
      ;; repeat until t3local-fun generates no more
      ((eq *emitted-local-funs* *local-funs*))
    ;; scan *local-funs* backwards
    (do ((lfs *local-funs* (cdr lfs)))
	((eq (cdr lfs) *emitted-local-funs*)
	 (setq *emitted-local-funs* lfs)
	 (locally (declare (notinline t3local-fun))
	   ;; so disassemble can redefine it
	   (t3local-fun (first lfs)))))))

(defun ctop-write (name h-pathname data-pathname
			&aux def top-output-string
			(*volatile* "volatile "))

  (setq *top-level-forms* (nreverse *top-level-forms*))
  (wt-nl "#include \"" (brief-namestring h-pathname) "\"")

  ;; VV might be needed by functions in CLINES.
  (wt-nl-h "#ifdef ECL_DYNAMIC_VV")
  (wt-nl-h "static cl_object *VV;")
  (wt-nl-h "#else")
  (wt-nl-h "static cl_object VV[VM];")
  (wt-nl-h "#endif")
  (output-clines *compiler-output2*)

  (wt-nl-h "#ifdef __cplusplus")
  (wt-nl-h "extern \"C\" {")
  (wt-nl-h "#endif")
  ;;; Initialization function.
  (let* ((*opened-c-braces* 0)
         (*aux-closure* nil)
	 (c-output-file *compiler-output1*)
	 (*compiler-output1* (make-string-output-stream))
	 (*emitted-local-funs* nil)
	 (*compiler-declared-globals* (make-hash-table)))
    (wt-nl "#include \"" (brief-namestring data-pathname) "\"")
    (wt-nl "#ifdef __cplusplus")
    (wt-nl "extern \"C\"")
    (wt-nl "#endif")
    (wt-nl "ECL_DLLEXPORT void " name "(cl_object flag)")
    (wt-nl-open-brace)
    (wt-nl "const cl_env_ptr cl_env_copy = ecl_process_env();")
    (wt-nl "cl_object value0;")
    (wt-nl "cl_object *VVtemp;")

    (wt-nl "if (flag != OBJNULL){")
    (wt-nl "Cblock = flag;")
    (wt-nl "#ifndef ECL_DYNAMIC_VV")
    (wt-nl "flag->cblock.data = VV;")
    (wt-nl "#endif")
    (when *self-destructing-fasl*
      (wt-nl "flag->cblock.self_destruct=1;"))
    (wt-nl "flag->cblock.data_size = VM;")
    (wt-nl "flag->cblock.temp_data_size = VMtemp;")
    (wt-nl "flag->cblock.data_text = compiler_data_text;")
    (wt-nl "flag->cblock.cfuns_size = compiler_cfuns_size;")
    (wt-nl "flag->cblock.cfuns = compiler_cfuns;")
    (when ext:*source-location*
      (wt-nl "flag->cblock.source = make_constant_base_string(\""
             (namestring (car ext:*source-location*)) "\");"))
    (wt-nl "return;}")
    (wt-nl "#ifdef ECL_DYNAMIC_VV")
    (wt-nl "VV = Cblock->cblock.data;")
    (wt-nl "#endif")
    ;; With this we ensure creating a constant with the tag
    ;; and the initialization file
    (wt-nl "Cblock->cblock.data_text = (const cl_object *)\"" (init-name-tag name) "\";")

    (wt-nl "VVtemp = Cblock->cblock.temp_data;")

    (wt-nl "ECL_DEFINE_SETF_FUNCTIONS")

    ;; Type propagation phase

    (when *do-type-propagation*
      (setq *compiler-phase* 'p1propagate)
      (dolist (form *top-level-forms*)
	(when form
	  (p1propagate form nil)))
      (dolist (fun *local-funs*)
        (p1propagate (fun-lambda fun) nil)))

    (setq *compiler-phase* 't2)

    (loop for form in (nconc (reverse *make-forms*) *top-level-forms*)
       do (emit-toplevel-form form c-output-file))
    (wt-nl-close-many-braces 0)
    (setq top-output-string (get-output-stream-string *compiler-output1*)))

  ;; Declarations in h-file.
  (wt-nl-h "static cl_object Cblock;")
  (let ((num-objects (data-size)))
    (if (zerop num-objects)
	(progn
	  (wt-nl-h "#undef ECL_DYNAMIC_VV")
	  (wt-nl-h "#define compiler_data_text 0")
	  (wt-nl-h "#define VM 0")
	  (wt-nl-h "#define VMtemp 0")
	  (wt-nl-h "#define VV NULL"))
	(progn
	  (wt-nl-h "#define VM " (data-permanent-storage-size))
	  (wt-nl-h "#define VMtemp "  (data-temporary-storage-size)))))

  (dolist (l *linking-calls*)
    (let* ((c-name (fourth l))
	   (var-name (fifth l)))
      (wt-nl-h "static cl_object " c-name "(cl_narg, ...);")
      (wt-nl-h "static cl_object (*" var-name ")(cl_narg, ...)=" c-name ";")))

  ;;; Global entries for directly called functions.
  (dolist (x *global-entries*)
    (apply 'wt-global-entry x))

  ;;; Initial functions for linking calls.
  (dolist (l *linking-calls*)
    (let* ((var-name (fifth l))
	   (c-name (fourth l))
	   (lisp-name (third l)))
      (wt-nl "static cl_object " c-name "(cl_narg narg, ...)"
	      "{TRAMPOLINK(narg," lisp-name ",&" var-name ",Cblock);}")))
  #+(or)
  (wt-nl-h "static cl_object ECL_SETF_DEFINITION(cl_object setf_vv, cl_object setf_form)
{
 cl_object f1 = ecl_fdefinition(setf_form);
 cl_object f2 = ECL_CONS_CAR(setf_vv);
 if (f1 != f2) {
  FEundefined_function(setf_form);
 }
 return f2;
}
")
  (wt-nl-h "#define ECL_DEFINE_SETF_FUNCTIONS ")
  (loop for (name setf-vv name-vv) in *setf-definitions*
     do (wt-h #\\ #\Newline setf-vv "=ecl_setf_definition(" name-vv ",ECL_T);"))

  (wt-nl-h "#ifdef __cplusplus")
  (wt-nl-h "}")
  (wt-nl-h "#endif")

  (when (and (listp *static-constants*)
             (setf *static-constants* (nreverse *static-constants*)))
    (wt-nl-h "/*")
    (wt-nl-h " * Statically defined constants")
    (wt-nl-h " */")
    (loop for (value name builder) in (reverse *static-constants*)
          do (terpri *compiler-output2*)
          do (funcall builder name value *compiler-output2*)))

  (output-cfuns *compiler-output2*)

  (setq *compiler-phase* 't3)

  ;;; Callbacks
  (when *callbacks*
    (wt-nl-h "#include <ecl/internal.h>")
    (dolist (x *callbacks*)
      (apply #'t3-defcallback x)))

  (wt-nl top-output-string))

(defun emit-toplevel-form (form c-output-file)
  (let ((*ihs-used-p* nil)
	(*max-lex* 0)
	(*max-env* 0)
	(*max-temp* 0)
	(*lcl* 0)
	(*lex* 0)
	(*level* 0)
	(*env* 0)
	(*env-lvl* 0)
	(*temp* 0)
	(*compile-to-linking-call* nil)
	(*compile-file-truename* (and form (c1form-file form)))
	(*compile-file-position* (and form (c1form-file-position form))))
    ;; We save the C body of the statement, indented, just in case
    ;; we need to add a {} section with the environment variables.
    (let ((body (let ((*opened-c-braces* (1+ *opened-c-braces*)))
		  (with-output-to-string (*compiler-output1*)
		    (t2expr form)))))
      (if (or (plusp *max-lex*)
	      (plusp *max-temp*)
	      (plusp *max-env*)
	      *ihs-used-p*)
	  (progn
	    (wt-nl-open-brace)
	    (wt-function-locals)
	    (write-sequence body *compiler-output1*)
	    (wt-nl-close-brace))
	  (write-sequence body *compiler-output1*)))
    (let ((*compiler-output1* c-output-file))
      (emit-local-funs))))

(defun c1eval-when (args)
  (check-args-number 'EVAL-WHEN args 1)
  (let ((load-flag nil)
	(compile-flag nil)
	(execute-flag nil))
    (dolist (situation (car args))
      (case situation
	((LOAD :LOAD-TOPLEVEL) (setq load-flag t))
	((COMPILE :COMPILE-TOPLEVEL) (setq compile-flag t))
	((EVAL :EXECUTE)
	 (if *compile-toplevel*
	     (setq compile-flag (or *compile-time-too* compile-flag))
	     (setq execute-flag t)))
	(otherwise (cmperr "The EVAL-WHEN situation ~s is illegal."
			   situation))))
    (cond ((not *compile-toplevel*)
	   (c1progn (and execute-flag (rest args))))
	  (load-flag
	   (let ((*compile-time-too* compile-flag))
	     (c1progn (rest args))))
	  (compile-flag
	   (cmp-eval (cons 'PROGN (rest args)))
	   (c1progn 'NIL))
	  (t
	   (c1progn 'NIL)))))

(defun t2compiler-let (c1form symbols values body)
  (declare (ignore c1form))
  (progv symbols values (c2expr body)))

(defun t2progn (c1form args)
  (declare (ignore c1form))
  (mapc #'t2expr args))

(defun exported-fname (name)
  (let (cname)
    (if (and (symbolp name) (setf cname (get-sysprop name 'Lfun)))
        (values cname t)
        (values (next-cfun "L~D~A" name) nil))))

;;; Mechanism for sharing code:
;;; FIXME! Revise this 'DEFUN stuff.
(defun new-defun (new &optional no-entry)
  #|
  (unless (fun-exported new)
    ;; Check whether this function is similar to a previous one and
    ;; share code with it.
    (dolist (old *global-funs*)
      (when (similar (fun-lambda new) (fun-lambda old))
	(cmpnote "Sharing code among functions ~A and ~A"
		 (fun-name new) (fun-name old))
	(setf (fun-shares-with new) old
	      (fun-cfun new) (fun-cfun old)
	      (fun-minarg new) (fun-minarg old)
	      (fun-maxarg new) (fun-maxarg old))
	(return))))
  |#
  (push new *global-funs*))

(defun print-function (x)
  (format t "~%<a FUN: ~A, CLOSURE: ~A, LEVEL: ~A, ENV: ~A>"
	  (fun-name x) (fun-closure x) (fun-level x) (fun-env x)))

(defmacro and! (&body body)
  `(let ((l (list ,@body)))
     (pprint (list* 'l? l))
     (every #'identity l)))

#|
(defun similar (x y)
  (let ((*processed* (make-hash-table :test #'equal)))
    ;; FIXME! This could be more accurate
    (labels ((similar (x y)
               (when (eql x y)
                 (return-from similar t))
               (let ((pair (cons x y)))
                 (case (gethash pair *processed* :not-found)
                   ((nil) (return-from similar nil))
                   ((t) (return-from similar t))
                   ((:ongoing) (return-from similar t))
                   ((:not-found)))
                 (setf (gethash pair *processed*) :ongoing)
                 (setf (gethash pair *processed*)
                       (and (eql (type-of x) (type-of y))
                            (typecase x
                              (CONS (and (similar (car x) (car y))
                                         (similar (cdr x) (cdr y))))
                              (VAR (similar-var x y))
                              (FUN (similar-fun x y))
                              (REF (similar-ref x y))
                              (TAG NIL)
                              (BLK NIL)
                              (C1FORM (similar-c1form x y))
                              (SEQUENCE (and (every #'similar x y)))
                              (T (equal x y)))))))
             (similar-list (x y)
               (null (set-difference x y)))
             (similar-ref (x y)
               (and (equal (ref-ref-ccb x) (ref-ref-ccb y))
                    (equal (ref-ref-clb x) (ref-ref-clb y))
                    (equal (ref-ref x) (ref-ref y))))
             (similar-var (x y)
               (and! (similar-ref x y)
                    (equal (var-name x) (var-name y))
                    (equal (var-kind x) (var-kind y))
                    (equal (var-loc x) (var-loc y))
                    (equal (var-type x) (var-type y))
                    (equal (var-index x) (var-index y))))
             (similar-c1form (x y)
               (and (equal (c1form-name x) (c1form-name y))
                    (similar (c1form-args x) (c1form-args y))
                    (similar (c1form-local-vars x) (c1form-local-vars y))
                    (eql (c1form-sp-change x) (c1form-sp-change y))
                    (eql (c1form-volatile x) (c1form-volatile y))))
             (similar-fun (x y)
               (and! (similar-ref x y)
                    (eql (fun-global x) (fun-global y))
                    (eql (fun-exported x) (fun-exported y))
                    (eql (fun-closure x) (fun-closure y))
                    (similar (fun-var x) (fun-var y))
                    (similar (fun-lambda x) (fun-lambda y))
                    (= (fun-level x) (fun-level y))
                    (= (fun-env x) (fun-env y))
                    (= (fun-minarg x) (fun-minarg y))
                    (eql (fun-maxarg x) (fun-maxarg y))
                    (every #'similar (fun-local-vars x) (fun-local-vars y))
                    (every #'similar (fun-referenced-vars x) (fun-referenced-vars y))
                    (every #'similar (fun-referenced-funs x) (fun-referenced-funs y))
                    (every #'similar (fun-child-funs x) (fun-child-funs y)))))
      (similar x y))))
|#

(defun wt-function-locals (&optional closure-type)
  ;; FIXME! Are we careful enough with temporary variables that
  ;; we need not make them volatile?
  (when (plusp *max-temp*)
    (wt-nl "cl_object ")
    (dotimes (i *max-temp*)
      (wt "T" i)
      (unless (= (1+ i) *max-temp*) (wt ", ")))
    (wt ";"))
  (when *ihs-used-p*
    (wt-nl "struct ecl_ihs_frame ihs;")
    (wt-nl "const cl_object _ecl_debug_env = ECL_NIL;"))
  ;; There should be no need to mark lex as volatile, since we
  ;; are going to pass pointers of this array around and the compiler
  ;; should definitely keep this in memory.
  (when (plusp *max-lex*)
    (wt-nl "volatile cl_object lex" *level* "[" *max-lex* "];"))
  (when (plusp *max-env*)
    (unless (eq closure-type 'CLOSURE)
      (wt-nl "cl_object " *volatile* "env0;"))
    ;; Note that the closure structure has to be marked volatile
    ;; or else GCC may optimize away writes into it because it
    ;; does not know it shared with the rest of the world.
    (when *aux-closure*
      (wt-nl "volatile struct ecl_cclosure aux_closure;"))
    (wt-nl "cl_object " *volatile*)
    (loop for i from 0 below *max-env*
       for comma = "" then ", "
       do (wt comma "CLV" i)
       finally (wt ";"))))

(defun wt-global-entry (fname cfun arg-types return-type)
    (when (and (symbolp fname) (get-sysprop fname 'NO-GLOBAL-ENTRY))
      (return-from wt-global-entry nil))
    (wt-comment-nl "global entry for the function ~a" fname)
    (wt-nl "static cl_object L" cfun "(cl_narg narg")
    (wt-nl-h "static cl_object L" cfun "(cl_narg")
    (do ((vl arg-types (cdr vl))
	 (lcl (1+ *lcl*) (1+ lcl)))
	((endp vl) (wt1 ")"))
      (declare (fixnum lcl))
      (wt1 ", cl_object ") (wt-lcl lcl)
      (wt-h ", cl_object"))
    (wt-h1 ");")
    (wt-nl-open-brace)
    (when (compiler-check-args)
      (wt-nl "_ecl_check_narg(" (length arg-types) ");"))
    (wt-nl "cl_env_copy->nvalues = 1;")
    (wt-nl "return " (case return-type
                            (FIXNUM "ecl_make_fixnum")
                            (CHARACTER "CODE_CHAR")
                            (DOUBLE-FLOAT "ecl_make_double_float")
                            (SINGLE-FLOAT "ecl_make_single_float")
			    #+long-float
                            (LONG-FLOAT "ecl_make_long_float")
                            (otherwise ""))
           "(LI" cfun "(")
    (do ((types arg-types (cdr types))
         (n 1 (1+ n)))
        ((endp types))
      (declare (fixnum n))
      (wt (case (car types)
            (FIXNUM "fix")
            (CHARACTER "ecl_char_code")
            (DOUBLE-FLOAT "df")
            (SINGLE-FLOAT "sf")
	    #+long-float
	    (LONG-FLOAT "ecl_long_float")
            (otherwise "")) "(")
        (wt-lcl n) (wt ")")
        (unless (endp (cdr types)) (wt ",")))
    (wt "));")
    (wt-nl-close-many-braces 0))

(defun rep-type (type)
  (case type
    (FIXNUM "cl_fixnum ")
    (CHARACTER "unsigned char ")
    (SINGLE-FLOAT "float ")
    (DOUBLE-FLOAT "double ")
    (otherwise "cl_object ")))

(defun t1ordinary (form)
  (when *compile-time-too* (cmp-eval form))
  (let ((*compile-toplevel* nil)
	(*compile-time-too* nil))
    (add-load-time-values (make-c1form* 'ORDINARY :args (c1expr form)))))

(defun p1ordinary (c1form assumptions form)
  (p1propagate form assumptions))

(defun t2ordinary (c1form form)
  (declare (ignore c1form))
  (let* ((*exit* (next-label))
	 (*unwind-exit* (list *exit*))
         (*destination* 'TRASH))
    (c2expr form)
    (wt-label *exit*)))

(defun add-load-time-values (form)
  (let ((previous (append (and (consp *load-time-values*)
			       (nreverse *load-time-values*))
			  (nreverse *make-forms*))))
    (when previous
      (setf *load-time-values* nil
	    *make-forms* nil)
      (setf form (make-c1form* 'PROGN :args (nconc previous (list form))))))
  form)

(defun t1defmacro (args)
  (check-args-number 'LOAD-TIME-VALUE args 2)
  (destructuring-bind (name lambda-list &rest body)
      args
    (multiple-value-bind (function pprint doc-string)
        (sys::expand-defmacro name lambda-list body)
      (let ((fn (cmp-eval function *cmp-env*)))
        (cmp-env-register-global-macro name fn))
      (t1expr* (macroexpand `(DEFMACRO ,@args))))))

(defun c1load-time-value (args)
  (check-args-number 'LOAD-TIME-VALUE args 1 2)
  (let ((form (first args))
	loc)
    (cond ((not (listp *load-time-values*))
	   ;; When using COMPILE, we set *load-time-values* to 'VALUES and
	   ;; thus signal that we do not want to compile these forms, but
	   ;; just to retain their value.
	   (return-from c1load-time-value (c1constant-value (cmp-eval form) :always t)))
          ((typep form '(or list symbol))
	   (setf loc (data-empty-loc))
	   (push (make-c1form* 'LOAD-TIME-VALUE :args loc (c1expr form))
		 *load-time-values*))
	  (t
	   (setf loc (add-object (cmp-eval form)))))
    (make-c1form* 'LOCATION :type t :args loc)))

(defun t2load-time-value (c1form vv-loc form)
  (declare (ignore c1form))
  (let* ((*exit* (next-label)) (*unwind-exit* (list *exit*))
         (*destination* vv-loc))
    (c2expr form)
    (wt-label *exit*)))

(defun t2make-form (c1form vv-loc form)
  (declare (ignore c1form))
  (let* ((*exit* (next-label)) (*unwind-exit* (list *exit*))
         (*destination* vv-loc))
    (c2expr form)
    (wt-label *exit*)))

(defun t2init-form (c1form vv-loc form)
  (declare (ignore c1form))
  (let* ((*exit* (next-label)) (*unwind-exit* (list *exit*))
         (*destination* 'TRASH))
    (c2expr form)
    (wt-label *exit*)))

(defun parse-cvspecs (x &aux (cvspecs nil))
  (dolist (cvs x (nreverse cvspecs))
    (cond ((symbolp cvs)
           (push (list :OBJECT (string-downcase (symbol-name cvs))) cvspecs))
          ((stringp cvs) (push (list :OBJECT cvs) cvspecs))
          ((and (consp cvs)
                (member (car cvs) '(OBJECT CHAR INT FLOAT DOUBLE)))
           (dolist (name (cdr cvs))
             (push (list (car cvs)
                         (cond ((symbolp name)
                                (string-downcase (symbol-name name)))
                               ((stringp name) name)
                               (t (cmperr "The C variable name ~s is illegal."
                                          name))))
                   cvspecs)))
          (t (cmperr "The C variable specification ~s is illegal." cvs))))
  )

(defun locative-type-from-var-kind (kind)
  (cdr (assoc kind
              '((:object . "_ecl_object_loc")
                (:fixnum . "_ecl_fixnum_loc")
                (:char . "_ecl_base_char_loc")
                (:float . "_ecl_float_loc")
                (:double . "_ecl_double_loc")
		#+sse2 (:int-sse-pack . "_ecl_int_sse_pack_loc")
		#+sse2 (:float-sse-pack . "_ecl_float_sse_pack_loc")
		#+sse2 (:double-sse-pack . "_ecl_double_sse_pack_loc")
                ((special global closure lexical) . NIL)))))

(defun build-debug-lexical-env (var-locations &optional first)
  #-:msvc ;; FIXME! Problem with initialization of statically defined vectors
  (let* ((filtered-locations '())
         (filtered-codes '()))
    ;; Filter out variables that we know how to store in the
    ;; debug information table. This excludes among other things
    ;; closures and special variables.
    (loop for var in var-locations
          for name = (let ((*package* (find-package "KEYWORD")))
                       (format nil "\"~S\"" (var-name var)))
          for code = (locative-type-from-var-kind (var-kind var))
          for loc = (var-loc var)
          when (and code (consp loc) (eq (first loc) 'LCL))
          do (progn
               (push (cons name code) filtered-codes)
               (push loc filtered-locations)))
    ;; Generate two tables, a static one with information about the
    ;; variables, including name and type, and dynamic one, which is
    ;; a vector of pointer to the variables.
    (when filtered-codes
      (setf *ihs-used-p* t)
      (wt-nl "static const struct ecl_var_debug_info _ecl_descriptors[]={")
      (loop for (name . code) in filtered-codes
            for i from 0
            do (wt-nl (if (zerop i) "{" ",{") name "," code "}"))
      (wt "};")
      (wt-nl "const cl_index _ecl_debug_info_raw[]={")
      (wt-nl (if first "(cl_index)(ECL_NIL)," "(cl_index)(_ecl_debug_env),")
             "(cl_index)(_ecl_descriptors)")
      (loop for var-loc in filtered-locations
            do (wt ",(cl_index)(&" var-loc ")"))
      (wt "};")
      (wt-nl "ecl_def_ct_vector(_ecl_debug_env,ecl_aet_index,_ecl_debug_info_raw,"
             (+ 2 (length filtered-locations))
             ",,);")
      (unless first
        (wt-nl "ihs.lex_env = _ecl_debug_env;")))
    filtered-codes))

(defun pop-debug-lexical-env ()
  (wt-nl "ihs.lex_env = _ecl_debug_env;"))

(defun t3local-fun (fun)
  (declare (type fun fun))

  ;; Compiler note about compiling this function
  (print-emitting fun)

  (let* ((lambda-expr (fun-lambda fun))
	 (*cmp-env* (c1form-env lambda-expr))
	 (*lcl* 0) (*temp* 0) (*max-temp* 0)
         (*last-label* 0)
	 (*lex* 0) (*max-lex* 0)
	 (*env* (fun-env fun))		; continue growing env
	 (*max-env* *env*) (*env-lvl* 0)
         (*aux-closure* nil)
	 (*level* (fun-lexical-levels fun))
	 (*exit* 'RETURN)
	 (*unwind-exit* '(RETURN))
	 (*destination* 'RETURN)
         (*ihs-used-p* nil)
	 (*opened-c-braces* 0)
	 (*tail-recursion-info* fun)
	 (*volatile* (c1form-volatile* lambda-expr)))
    ;; Function declaration. Returns NIL if this function needs no body.
    (when (t3local-fun-declaration fun)
      (wt-nl-open-brace)
      (let ((body (t3local-fun-body fun)))
	(wt-function-locals (fun-closure fun))
	(wt-nl "const cl_env_ptr cl_env_copy = ecl_process_env();")
	(when (eq (fun-closure fun) 'CLOSURE)
	  (wt-nl "cl_object " *volatile* "env0 = cl_env_copy->function->cclosure.env;"))
	(wt-nl "cl_object " *volatile* "value0;")
	(when (policy-check-stack-overflow)
	  (wt-nl "ecl_cs_check(cl_env_copy,value0);"))
	(when (eq (fun-closure fun) 'CLOSURE)
	  (t3local-fun-closure-scan fun))
	(write-sequence body *compiler-output1*)
	(wt-nl-close-many-braces 0)))))

(defun t3local-fun-body (fun)
  (let ((string (make-array 2048 :element-type 'base-char
			    :adjustable t
			    :fill-pointer 0)))
    (with-output-to-string (*compiler-output1* string)
      (let ((lambda-expr (fun-lambda fun)))
	(c2lambda-expr (c1form-arg 0 lambda-expr)
		       (c1form-arg 2 lambda-expr)
		       (fun-cfun fun)
		       (fun-name fun)
		       (fun-needs-narg fun)
		       (fun-required-lcls fun)
		       (fun-closure fun))))
    string))

(defun t3local-fun-declaration (fun)
  (declare (type fun fun))
  (wt-comment-nl (cond ((fun-global fun) "function definition for ~a")
                       ((eq (fun-closure fun) 'CLOSURE) "closure ~a")
                       (t "local function ~a"))
                 (or (fun-name fun) (fun-description fun) 'CLOSURE))
  (when (fun-shares-with fun)
    (wt-comment-nl "... shares definition with ~a" (fun-name (fun-shares-with fun)))
    (return-from t3local-fun-declaration nil))
  (let* ((comma "")
	 (lambda-expr (fun-lambda fun))
	 (volatile (c1form-volatile* lambda-expr))
	 (lambda-list (c1form-arg 0 lambda-expr))
	 (requireds (mapcar #'(lambda (v) (next-lcl (var-name v)))
			    (car lambda-list)))
	 (narg (fun-needs-narg fun)))
    (let ((cmp-env (c1form-env lambda-expr)))
      (wt-comment-nl "optimize speed ~D, debug ~D, space ~D, safety ~D "
		     (cmp-env-optimization 'speed cmp-env)
		     (cmp-env-optimization 'debug cmp-env)
		     (cmp-env-optimization 'space cmp-env)
		     (cmp-env-optimization 'safety cmp-env)))
    (let ((cfun (fun-cfun fun)))
      (cond ((fun-exported fun)
	     (wt-nl-h "ECL_DLLEXPORT cl_object " cfun "(")
	     (wt-nl "cl_object " cfun "("))
	    (t
	     (wt-nl-h "static cl_object " cfun "(")
	     (wt-nl "static cl_object " cfun "("))))
    (when narg
      (wt-h volatile "cl_narg")
      (wt volatile "cl_narg narg")
      (setf comma ", "))
    (dotimes (n (fun-lexical-levels fun))
      (wt-h comma "volatile cl_object  *")
      (wt comma "volatile cl_object *lex" n)
      (setf comma ", "))
    (loop for lcl in (setf (fun-required-lcls fun) requireds)
       do (wt-h comma "cl_object " volatile)
	 (wt comma "cl_object " volatile lcl)
	 (setf comma ", "))
    (when narg
      (wt-h ", ...")
      (wt ", ..."))
    (wt-h ");")
    (wt ")"))
  t)

(defun fun-closure-variables (fun)
  (sort (remove-if
	 #'(lambda (x)
	     (or
	      ;; non closure variable
	      (not (ref-ref-ccb x))
	      ;; special variable
	      (eq (var-kind x) 'special)
	      ;; not actually referenced
	      (and (not (var-referenced-in-form x (fun-lambda fun)))
		   (not (var-changed-in-form x (fun-lambda fun))))
	      ;; parameter of this closure
	      ;; (not yet bound, therefore var-loc is OBJECT)
	      (eq (var-loc x) 'OBJECT)))
	 (fun-referenced-vars fun))
	#'>
	:key #'var-loc))

(defun fun-lexical-levels (fun)
  (if (eq (fun-closure fun) 'LEXICAL)
      (fun-level fun)
      0))

(defun t3local-fun-closure-scan (fun)
  (let ((clv-used (fun-closure-variables fun)))
    (wt-nl "/* Scanning closure data ... */")
    (do ((n (1- (fun-env fun)) (1- n))
	 (bs clv-used)
	 (first t))
	((or (minusp n) (null bs)))
      (wt-nl "CLV" n)
      (if first
	  (progn (wt " = env0;") (setf first nil))
	  (wt " = _ecl_cdr(CLV" (1+ n) ");"))
      (when (= n (var-loc (first bs)))
	(wt-comment (var-name (first clv-used)))
	(pop clv-used)))
    (wt-nl-open-brace)
    (wt " /* ... closure scanning finished */")))

;;; ----------------------------------------------------------------------
;;; Optimizer for FSET. Removes the need for a special handling of DEFUN as a
;;; toplevel form and also allows optimizing calls to DEFUN or DEFMACRO which
;;; are not toplevel, but which create no closures.
;;;
;;; The idea is as follows: when the function or macro to be defined is not a
;;; closure, we can use the auxiliary C functions c_def_c_*() instead of
;;; creating a closure and invoking si_fset(). However until the C2 phase of
;;; the compiler we do not know whether a function is a closure, hence the need
;;; for a c2fset.
;;;
;;; We optimize (SYS:FSET #'(LAMBDA ...) ..) and also, accidentally,
;;; (SYS:FSET (FLET ((FOO ...)) #'FOO) ...) which is to what LAMBDA gets
;;; translated in c1function.
;;;
(defun t1fset (args)
  (let ((form `(si::fset ,@args)))
    (when *compile-time-too*
      (cmp-eval form))
    (let ((*compile-toplevel* nil)
	  (*compile-time-too* nil))
      (add-load-time-values (c1fset form)))))

(defun c1fset (form)
  (destructuring-bind (fname def &optional (macro nil) (pprint nil))
      (rest form)
    (let* ((*use-c-global* t)
	   (fun-form (c1expr def)))
      (when (eq (c1form-name fun-form) 'LOCALS)
	(let* ((function-list (c1form-arg 0 fun-form))
	       (fun-object (pop function-list))
	       (form (c1form-arg 1 fun-form))
	       (labels (c1form-arg 2 fun-form)))
	  (when (and
		 ;; Only 1 function
		 (null function-list)
		 ;; Not closed over anything
		 (every #'global-var-p (fun-referenced-vars fun-object))
		 ;; Referencing the function variable
		 (eq (c1form-name form) 'VAR)
		 (eq (c1form-arg 0 form)
		     (fun-var fun-object)))
	    (when (fun-no-entry fun-object)
	      (when macro
		(cmperr "Declaration C-LOCAL used in macro ~a"
                        (fun-name fun-object)))
	      (return-from c1fset
		(make-c1form* 'SI:FSET :args fun-object nil nil nil nil)))
	    (when (and (typep macro 'boolean)
		       (typep pprint '(or integer null))
		       (consp fname)
		       (eq (first fname) 'quote))
	      (return-from c1fset
		(make-c1form* 'SI:FSET :args
			      fun-object ;; Function object
			      (let* ((fname (second fname))
				     (in-cl-symbols-p (and (symbolp fname)
							   (si::mangle-name fname))))
				(add-object fname :permanent t
					    :duplicate in-cl-symbols-p
					    :used-p t))
			      macro
			      pprint
			      ;; The c1form, when we do not optimize
			      (list (c1expr fname)
				    fun-form
				    (c1expr macro)
				    (c1expr pprint)))))))))
    (t1ordinary form)))

(defun p1fset (c1form assumptions fun fname macro pprint c1forms)
  (p1propagate (fun-lambda fun) assumptions))

(defun t2fset (c1form &rest args)
  (t2ordinary nil c1form))

(defun c2fset (c1form fun fname macro pprint c1forms)
  (when (fun-no-entry fun)
    (wt-nl "(void)0; /* No entry created for "
	   (format nil "~A" (fun-name fun))
	   " */")
    ;; FIXME! Look at C2LOCALS!
    (new-local fun)
    (return-from c2fset))
  (unless (and (not (fun-closure fun))
	       (eq *destination* 'TRASH))
    (return-from c2fset
      (c2call-global c1form 'SI:FSET c1forms)))
  (let ((*inline-blocks* 0)
	(loc (data-empty-loc)))
    (push (list loc fname fun) *global-cfuns-array*)
    ;; FIXME! Look at C2LOCALS!
    (new-local fun)
    (wt-nl (if macro "ecl_cmp_defmacro(" "ecl_cmp_defun(")
	   loc ");")
    (wt-comment (loc-immediate-value fname))
    (close-inline-blocks)))

(defun output-cfuns (stream)
  (let ((n-cfuns (length *global-cfuns-array*)))
    (wt-nl-h "/*")
    (wt-nl-h " * Exported Lisp functions")
    (wt-nl-h " */")
    (wt-nl-h "#define compiler_cfuns_size " n-cfuns)
    (if (zerop n-cfuns)
        (wt-nl-h "#define compiler_cfuns NULL")
        (progn
          (format stream "~%static const struct ecl_cfun compiler_cfuns[] = {~
~%~t/*t,m,narg,padding,name,block,entry*/");
          (loop for (loc fname-loc fun) in (nreverse *global-cfuns-array*)
                do (let* ((cfun (fun-cfun fun))
                          (minarg (fun-minarg fun))
                          (maxarg (fun-maxarg fun))
                          (narg (if (and (= minarg maxarg)
					 (<= maxarg si:c-arguments-limit))
				    maxarg
				    -1)))
                     (format stream "~%{0,0,~D,0,ecl_make_fixnum(~D),ecl_make_fixnum(~D),(cl_objectfn)~A,ECL_NIL,ecl_make_fixnum(~D)},"
                             narg
                             (vv-location loc)
                             (vv-location fname-loc)
                             cfun (fun-file-position fun))))
          (format stream "~%};")))))
