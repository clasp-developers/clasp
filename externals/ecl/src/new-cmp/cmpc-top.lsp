;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 2009, Juan Jose Garcia-Ripoll
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;
;;;;  CMPC-TOP -- Dump all lisp forms and data
;;;;

(in-package "C-BACKEND")

(defun ctop-write (name h-pathname data-pathname
                   &key shared-data input-designator
                   &aux def top-output-string
                   (*volatile* "volatile "))

  ;; Output some information about the compiler at the top of the sources.
  ;;
  (wt-comment-nl "Compiler: ~A ~A"
                 (lisp-implementation-type)
                 (lisp-implementation-version))
  #-ecl-min
  (multiple-value-bind (second minute hour day month year)
      (get-decoded-time)
    (wt-comment-nl "Date: ~D/~D/~D ~2,'0D:~2,'0D (yyyy/mm/dd)"
                   year month day hour minute)
    (wt-comment-nl "Machine: ~A ~A ~A"
                   (software-type) (software-version) (machine-type)))
  (wt-comment-nl "Source: ~A"
                 (or input-designator "Uknown"))

  (wt-nl1 "#include " c::*cmpinclude*)
  (wt-nl1 "#include \"" (si::coerce-to-filename h-pathname) "\"")
  ;; All lines from CLINES statements are grouped at the beginning of the header
  ;; Notice that it does not make sense to guarantee that c-lines statements
  ;; are produced in-between the function definitions, because two functions
  ;; might be collapsed into one, or we might not produce that function at all
  ;; and rather inline it.
  (do ()
      ((null *clines-string-list*))
    (wt-nl-h (pop *clines-string-list*)))
  (wt-nl-h "#ifdef __cplusplus")
  (wt-nl-h "extern \"C\" {")
  (wt-nl-h "#endif")
  (when si::*compiler-constants*
    (wt-nl-h "#include <string.h>"))
  (unless shared-data
    (wt-nl1 "#include \"" (si::coerce-to-filename data-pathname) "\""))
  ;;; Initialization function.
  (let* ((c-output-file *compiler-output1*)
	 (*compiler-output1* (make-string-output-stream))
	 (*compiler-declared-globals* (make-hash-table)))

    ;; Type propagation phase
    (when *do-type-propagation*
      (setq *compiler-phase* 'p1propagate)
      (dolist (form *top-level-forms*)
        (p1propagate form nil))
      (dolist (fun *local-funs*)
        (propagate-function-types fun)))

    (setq *compiler-phase* 't2)

    ;; Optimization passes
    (c-backend-passes)

    ;; Emit entry function
    (let ((*compile-to-linking-call* nil))
      (t3local-fun *top-level-forms*))

    ;; Now emit the rest
    (let ((*compiler-output1* c-output-file))
      (emit-local-funs *top-level-forms*))

    (setq top-output-string (get-output-stream-string *compiler-output1*)))

  ;; Declarations in h-file.
  (wt-nl-h "static cl_object Cblock;")

  (dolist (l *linking-calls*)
    (let* ((c-name (fourth l))
	   (var-name (fifth l)))
      (wt-nl-h "static cl_object " c-name "(cl_narg, ...);")
      (wt-nl-h "static cl_object (*" var-name ")(cl_narg, ...)=" c-name ";")))

  ;;; Initial functions for linking calls.
  (dolist (l *linking-calls*)
    (let* ((var-name (fifth l))
	   (c-name (fourth l))
	   (lisp-name (third l)))
      (wt-nl1 "static cl_object " c-name "(cl_narg narg, ...)"
	      "{TRAMPOLINK(narg," lisp-name ",&" var-name ",Cblock);}")))

  (wt-nl-h "#ifdef __cplusplus")
  (wt-nl-h "}")
  (wt-nl-h "#endif")

  (output-cfuns *compiler-output2*)

  (setq *compiler-phase* 't3)

  ;;; Callbacks
  (when *callbacks*
    (wt-nl-h "#include <ecl/internal.h>")
    (dolist (x *callbacks*)
      (apply #'t3-defcallback x)))

  (wt-nl top-output-string))

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
                          (narg (if (= minarg maxarg) maxarg nil)))
                     (format stream "~%{0,0,~D,0,MAKE_FIXNUM(~D),MAKE_FIXNUM(~D),(cl_objectfn)~A,Cnil,MAKE_FIXNUM(~D)},"
                             (or narg -1) (second loc) (second fname-loc)
                             cfun (fun-file-position fun))))
          (format stream "~%};")))))

(defun emit-local-funs (fun)
  (loop with *compile-time-too* = nil
     with *compile-toplevel* = nil
     with emitted-local-funs = (make-hash-table :test #'eql)
     with pending = (fun-child-funs fun)
     while pending
     do (let ((f (pop pending)))
          (when (gethash f emitted-local-funs)
            (error "Doubly emitted function ~A" f))
          (t3local-fun f)
          (setf (gethash f emitted-local-funs) t
                pending (append (fun-child-funs f) pending)))))

(defun t3local-fun (fun)
  (print-emitting fun)
  (let* ((*current-function* fun)
         (*compile-file-truename* (fun-file fun))
         (*compile-file-position* (fun-file-position fun))
         (*current-toplevel-form* (fun-toplevel-form fun))
         (*lcl* (fun-last-lcl fun))
         (*last-label* (fun-last-label fun))
	 (*lex* 0)
         (*max-lex* 0)
	 (*env* (fun-env fun))		; continue growing env
	 (*max-env* *env*)
         (*env-lvl* 0)
	 (*level* (if (eq (fun-closure fun) 'LEXICAL)
                      (fun-level fun)
                      0))
         (*volatile* (if (fun-volatile-p fun) "volatile " ""))
         (*permanent-data* t))
    (c2driver (fun-lambda fun))))

