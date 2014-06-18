;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  cmdline.lsp -- command line processing
;;;;
;;;;  Copyright (c) 2005, Juan Jose Garcia-Ripoll
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;

(in-package "SYSTEM")

(defparameter *command-break-enable* nil)

(defparameter *lisp-init-file-list* '("~/.ecl" "~/.eclrc")
  "List of files automatically loaded when ECL is invoked.")

(defparameter *help-message* "
Usage: ecl [-? | --help]
           [-dir dir] [-load file] [-shell file] [-eval expr] [-rc | -norc] [-hp | -nohp]
           [--c-stack size] [--lisp-stack size] [--heap-size size] [--frame-stack size]
           [[-o ofile] [-c [cfile]] [-h [hfile]] [-data [datafile]] [-s] [-q]
            -compile file]
           [[-o ofile] -link file+]
           [--input-encoding external-format] [--output-encoding external-format]
           [--error-encoding external-format] [--encoding external-format]
"
 "Prints a help message about command line arguments of ECL")

(defun command-args ()
  "Returns the original command line arguments as list. First argument is
the ECL program itself."
  (loop for i from 0 below (argc)
           collect (argv i)))

(defparameter *command-args*
  (command-args))

(defparameter *unprocessed-ecl-command-args*
  nil
  "As part of ECL's command line rules, this variable stores all arguments which
appeared after a '--'.")

(defun command-arg-error (str &rest fmt-args)
  ;; Format an error message and quit
  (declare (si::c-local))
  (apply #'format *error-output* str fmt-args)
  (princ *help-message* *error-output*)
  (quit 1))

(defconstant +default-command-arg-rules+
  '(("--help" 0 #0=(progn (princ *help-message* *standard-output*) (quit)) :noloadrc)
    ("-?" 0 #0# :noloadrc)
    ("-norc" 0 nil :noloadrc)
    ("--version" 0
     (progn (setf quit 0)
            (format *standard-output* "ECL ~A~%" (lisp-implementation-version)))
     :noloadrc)
    ("-debug" 0 (setf *command-break-enable* t))
    ("-nodebug" 0 (setf *command-break-enable* nil))
    ("-eval" 1 (eval (read-from-string 1)))
    ("-shell" 1 (progn (setq quit 0)
		       (setq ext:*unprocessed-ecl-command-args* (rest 1))
		       (load (first (rest 1)) :verbose nil))
     :stop)
    ("-load" 1 (load 1 :verbose verbose))
    ("-dir" 1 (setf (logical-pathname-translations "SYS")
	       `(("**;*.*" ,(merge-pathnames "**/*.*" (truename 1))))))
    ("--heap-size" 1 (ext:set-limit 'ext:heap-size (read-from-string 1)))
    ("--lisp-stack" 1 (ext:set-limit 'ext:lisp-stack (read-from-string 1)))
    ("--frame-stack" 1 (ext:set-limit 'ext:frame-stack (read-from-string 1)))
    ("--c-stack" 1 (ext:set-limit 'ext:c-stack (read-from-string 1)))
    ("--trap-fpe" 0 (si::trap-fpe t t))
    ("--no-trap-fpe" 0 (si::trap-fpe t nil))
    ("--encoding" 1 (dolist (i (list *standard-input* *standard-output*
                                     *error-output* *trace-output*))
                      (setf (stream-external-format i) (read-from-string 1))))
    ("--input-encoding" 1 (setf (stream-external-format *standard-input*)
                           (read-from-string 1)))
    ("--output-encoding" 1 (setf (stream-external-format *standard-output*)
                            (read-from-string 1)))
    ("--error-encoding" 1 (setf (stream-external-format *error-output*)
                           (read-from-string 1)))
    ("-compile" 1
     (progn
       (setq quit
	     (if (nth-value 3
		     (compile-file 1 :output-file output-file :c-file c-file
				   :h-file h-file :data-file data-file
				   :verbose verbose :system-p system-p))
		 1
		 0)
	     output-file t
	     c-file nil
	     h-file nil
	     data-file nil
	     system-p nil)))
    ("-link" &rest
     (progn
       (require 'cmp)
       (funcall (read-from-string "c::build-program")
		(or output-file "lisp.exe") :lisp-files '&rest)
       (setq output-file t quit t)))
    ("-o" &optional (setq output-file 1))
    ("-c" &optional (setq c-file 1))
    ("-h" &optional (setq h-file 1))
    ("-data" 1 (setq data-file 1))
    ("-q" 0 (setq verbose nil))
    ("-hp" 0 (setf *relative-package-names* t))
    ("-nohp" 0 (setf *relative-package-names* nil))
    ("-s" 0 (setq system-p t))
    ("--" 1 (setf ext:*unprocessed-ecl-command-args* (rest 1)) :stop)))

(defun produce-init-code (option-list rules)
  (do* ((commands '())
        (stop nil)
	(loadrc t))
       ((or stop (null option-list))
	(values `(let ((output-file t)
		       (c-file nil)
		       (h-file nil)
		       (data-file nil)
		       (verbose t)
		       (system-p nil)
		       (quit nil)
                       (*command-break-enable* nil))
		   ,@(nreverse commands)
		   (when quit (quit 0)))
		loadrc
		option-list))
    (let* ((option (pop option-list))
	   (rule (assoc option rules :test #'string=)))
      (unless rule
	;; If there is a default rule, group all remaining arguments
	;; including the unmatched one, and pass them to this rule.
	(setf rule (assoc "*DEFAULT*" rules :test #'string=)
	      stop t)
	(unless rule
	  (command-arg-error "Unknown command line option ~A.~%" option)))
      (case (fourth rule)
        (:noloadrc (setf loadrc nil))
        (:loadrc (setf loadrc t))
        (:stop (setf option-list `('(,option ,@option-list))
                     stop t)))
      (let ((pattern (copy-tree (third rule)))
            (noptions (second rule)))
	(cond ((equal noptions 0)
	       ;; No extra arguments
	       )
	      ((and (equal noptions '&optional)
		    (or (null option-list)
			(assoc (first option-list) rules :test #'string=)))
	       ;; The argument is optional and the next command line option is
	       ;; either absent or it is a valid command line option
	       (nsubst t 1 pattern))
	      ((null option-list)
	       (command-arg-error
		"Missing argument after command line option ~A.~%"
		option))
	      ((or (eq noptions 'rest) (eq noptions '&rest))
	       (nsubst option-list noptions pattern)
	       (setf option-list nil))
	      (t
	       (nsubst (pop option-list) 1 pattern)))
        (push pattern commands)))))

(defun process-command-args (&key
			     (args (rest *command-args*))
			     (rules +default-command-arg-rules+))
"PROCESS-COMMAND-ARGS takes a list of arguments and processes according
to a set of rules. These rules are of the format

	(option-name nargs template [ :stop | :noloadrc | :loadrc ] )

OPTION-NAME is a string containing the command line option. NARGS is
the number of arguments that this option takes. TEMPLATE is a lisp
form where numbers from 0 to NARGS will be substituted by the
arguments, and which will be evaluated afterwards. The flags :STOP,
:NOLOADRC and :LOADRC denote whether to stop processing the command
line after this option and whether the initialization file will be
loaded before evaluating all forms.

An excerpt of the rules used by ECL:
'((\"--help\" 0 #0=(progn (princ *help-message* *standard-output*) (quit))
               :noloadrc)
  (\"-?\" 0 #0# :noloadrc)
  (\"-h\" 0 #0# :noloadrc)
  (\"-norc\" 0 nil :noloadrc)
  (\"--\" 0 nil :stop)
  (\"-eval\" 1 (eval (read-from-string 1))))
"
  (multiple-value-bind (commands loadrc unprocessed-options)
      (produce-init-code args rules)
    (declare (ignore unprocessed-options))
    (restart-case
        (handler-bind ((error
                        #'(lambda (c)
                            (if *command-break-enable*
                                (invoke-debugger c)
                                (progn
                                  (format *error-output*
                                          "An error occurred during initialization:~%~A.~%"
                                          c)
                                  (quit 1))))))
          (progn
            (when loadrc
              (dolist (file *lisp-init-file-list*)
                (when (load file :if-does-not-exist nil :search-list nil :verbose nil)
                  (return))))
            (eval commands)))
      (continue ()
        :report "Ignore initialization errors and continue.")
      (abort ()
        :report "Quit ECL unsafely, ignoring all existing threads."
        (si::quit -1 nil)))))
