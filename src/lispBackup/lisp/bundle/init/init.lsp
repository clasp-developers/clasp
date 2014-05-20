;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;


;; todo fix this --> #+(or)( '*)

;(load "../cmp/init")
(debug-log-off)
(setq *print-repl-read* nil)
(setq *load-print* nil)
(setq *print-source-code-cons* nil) ;; set to t if you want parsePos info in conses

(setq *features* (cons :cando *features*))
(setq *features* (cons :ecl-min *features*))
(setq *features* (cons :scicl-min *features*))
(setq *features* (cons :ecl *features*))
;;(setq *features* (cons :clos *features*))

(make-package :clos :use '(:core) )
(make-package :ext :use '(:core) )
(make-package "COMPILER" :use '(:core) :nicknames '(:cmp))

(make-package 'newcl :nicknames '( "newcl" "NEWCL" ) :use '( :core ) )



(select-package :core)

(export '(load compile eval))

(select-package :ext)
(*make-special '*register-with-pde-hook*)
(setq *register-with-pde-hook* ())
(*make-special '*source-location*)
(setq *source-location* nil)
(export '*register-with-pde-hook*)
(*fset 'register-with-pde
      #'(lambda (whole env)
	  (let* ((definition (second whole))
		 (output-form (third whole)))
	    `(if ext:*register-with-pde-hook*
		 (funcall ext:*register-with-pde-hook*
			  (copy-tree *source-location*)
			  ,definition
			  ,output-form)
		 ,output-form)))
      t)




(import 'core:lambda-block)

(select-package :core)
(use-package :compiler)


(si::*fset 'core::defvar #'(lambda (whole env)
			    (let ((var (cadr whole))
				  (form (caddr whole))
				  (doc-string (cadddr whole)))
				  "Syntax: (defparameter name form [doc])
Declares the global variable named by NAME as a special variable and assigns
the value of FORM to the variable.  The doc-string DOC, if supplied, is saved
as a VARIABLE doc and can be retrieved by (documentation 'NAME 'variable)."
				  `(LOCALLY (DECLARE (SPECIAL ,var))
				     (SYS:*MAKE-SPECIAL ',var)
				     ,@(if form
					     `((if (boundp ',var)
						   nil
						   (setq ,var ,form)))))))
	  t )
(export 'defvar)

(si::*fset 'core::defparameter #'(lambda (whole env)
			    (let ((var (cadr whole))
				  (form (caddr whole))
				  (doc-string (cadddr whole)))
				  "Syntax: (defparameter name form [doc])
Declares the global variable named by NAME as a special variable and assigns
the value of FORM to the variable.  The doc-string DOC, if supplied, is saved
as a VARIABLE doc and can be retrieved by (documentation 'NAME 'variable)."
				  `(LOCALLY (DECLARE (SPECIAL ,var))
				     (SYS:*MAKE-SPECIAL ',var)
				     (SETQ ,var ,form))))
	  t )
(export 'defparameter)


(si::*fset 'core::defconstant #'(lambda (whole env)
			    (let ((var (cadr whole))
				  (form (caddr whole))
				  (doc-string (cadddr whole)))
				  "Syntax: (defconstant name form [doc])
Declares the global variable named by NAME as a special variable and assigns
the value of FORM to the variable.  The doc-string DOC, if supplied, is saved
as a VARIABLE doc and can be retrieved by (documentation 'NAME 'variable)."
				  `(LOCALLY (DECLARE (SPECIAL ,var))
				     (SYS:*MAKE-SPECIAL ',var)
				     (SETQ ,var ,form))))
	  t )
(export 'defconstant)


(si::*fset 'defun
	  #'(lambda (def env)
	      (let* ((name (second def))
		     (func `(function (ext::lambda-block ,@(cdr def)))))
		(ext:register-with-pde def `(si::*fset ',name ,func))))
	  t)
(export '(defun))


;; This is used extensively in the ecl compiler and once in predlib.lsp
(defparameter *alien-declarations* ())




(defun get-path-with-extension (module &optional (extension ".lsp"))
  (make-path (bformat nil "%s/CANDO/init/%s%s"
		      (as-string (script-dir))
		      (string-downcase (symbol-name module))
		      extension)
	     ))





(si::*fset 'interpreter-iload
	  #'(lambda (module &aux (path (get-path-with-extension module)))
	      (let ((name (as-string path)))
		(bformat t "Loading interpreted file: %s\n" name)
		(load name)))
	  nil)


(interpreter-iload 'cmp/jit-setup)

#| If we aren't using the compiler then just load everything with the interpreter |#
(si::*fset 'iload #'interpreter-iload)


(defparameter *reversed-init-filenames* ())

#| Otherwise use the compiler |#
#-nocmp
(defun iload (fn &key load-bitcode)
  (setq *reversed-init-filenames* (cons fn *reversed-init-filenames*))
  (let* ((lsp-path (get-path-with-extension fn ".lsp"))
	 (bc-path (get-path-with-extension fn ".bc"))
	 (load-bc (if (not (exists bc-path))
		      (progn
;;			(bformat t "Bitcode file %s doesn't exist\n" (path-file-name bc-path))
			nil)
		      (if load-bitcode
			  (progn
;;			    (bformat t "Bitcode file %s exists - force loading\n" (path-file-name bc-path))
			    t)
			  (let ((bc-newer (> (last-write-time bc-path) (last-write-time lsp-path))))
			    (if bc-newer
				(progn
;;				  (bformat t "Bitcode recent - loading %s\n" (path-file-name bc-path))
				  t)
				(progn
;;				  (bformat t "Bitcode file %s is older than lsp file - loading lsp file\n" (path-file-name bc-path))
				  nil)))))))
    (if load-bc
	(progn
	  (bformat t "Loading bitcode file: %s\n" (path-file-name bc-path))
	  (bcload (as-string bc-path)))
	(progn
	  (bformat t "Loading interpreted file: %s\n" (path-file-name lsp-path))
	  (load (as-string lsp-path)))))
  )




(defun delete-init-file (module &key (really-delete t))
  (let ((bitcode-path (get-path-with-extension module ".bc")))
    (bformat t "Module: %s\n" module)
    (if (exists bitcode-path)
	(if really-delete
	    (progn
	      (bformat t "     Deleting: %s\n" (path-file-name bitcode-path))
	      (delete-file bitcode-path)
	      ))
	(bformat t "    File doesn't exist: %s\n" (path-file-name bitcode-path))
	)))



(defun compile-iload (filename &key (then-load-it t) load-bitcode (recompile nil))
  (let* ((source-path (get-path-with-extension filename ".lsp"))
	 (bitcode-path (get-path-with-extension filename ".bc"))
	 (load-bitcode (if (exists bitcode-path)
			   (if load-bitcode
			       t
			       (if (> (last-write-time bitcode-path)
				      (last-write-time source-path))
				   t
				   nil))
			   nil)))
    (if (and load-bitcode (not recompile))
	(progn
	  (bformat t "Skipping compilation of %s - it's bitcode file is more recent\n" (path-file-name source-path))
	  (bformat t "   Loading the compiled file: %s\n" (path-file-name bitcode-path))
	  (bcload (as-string bitcode-path)))
	(progn
	  (bformat t "Compiling %s\n" (path-file-name source-path))
	  (print (list "Current package = " *package*))
	  (cmp:compile-file (as-string source-path) :output-file (as-string bitcode-path) :print t :verbose t)
	  (if then-load-it
	      (progn
		(bformat t "    Loading newly compiled file: %s\n" (path-file-name bitcode-path))
		(bcload (as-string bitcode-path))))
	  ))))





(defparameter *init-files*
  '(
    lsp/foundation
    lsp/export
    lsp/defmacro
    lsp/helpfile
    lsp/evalmacros
    lsp/logging
    lsp/makearray
    :tiny

    :pre-cmp

    cmp/cmpsetup
    cmp/cmpglobals
    cmp/cmpvar
    cmp/compile-main
    cmp/llvm-ir
    cmp/exception-handling
    cmp/debuginfo
    cmp/lambda-list
    cmp/compile-var-lookups
    cmp/cmpquote
    cmp/compiler
    cmp/compile-file

    :cmp


    ;;"top"
    lsp/setf
    lsp/setfrest
    lsp/predlib
    lsp/sharpmacros
    lsp/seqmacros
    lsp/seqlib
    lsp/seq
    lsp/listlib
    lsp/cmuutil
;;    lsp/arraylib
    lsp/assert
    lsp/defstruct
    lsp/iolib
    lsp/module
    ;; needs clos --> restarts --> conditions
;;    lsp/assert
    ;;    cmp/conditions
    ;; "format" ;; ----- very slow to load, leave it out for now

    :pre-loop

;;    lsp/lewp2
    lsp/loop2
    :loop
    lsp/format
    :front


    #|
    arraylib
    describe
    iolib
    listlib
    mislib
    numlib
    packlib
    trace
    |#

    lsp/packlib
    lsp/defpackage
    :pre-clos

;;    closette/closette

    clos/package
    clos/cpl
    clos/hierarchy
    clos/std-slot-value
    clos/slot


    clos/boot
    clos/kernel
    clos/method
    clos/combin
    clos/std-accessors
    clos/defclass
    clos/slotvalue
    clos/standard
    clos/builtin
    clos/change
    clos/stdmethod
    clos/generic
    clos/fixup
    clos/conditions
    clos/print
    clos/streams

    :clos

;;    lsp/pprint
))


(defun select-source-files (last-file &key (all-files *init-files*))
  (let ((cur all-files)
	files file)
    (tagbody
     top
       (setq file (car cur))
       (if (endp cur) (go done))
       (if last-file
	   (if (eq last-file file)
	       (progn
		 (if (not (keywordp file))
		     (setq files (cons file files)))
		 (go done))))
       (if (not (keywordp file)) 
	   (setq files (cons file files)))
       (setq cur (cdr cur))
       (go top)
     done
       )
    (nreverse files)
    ))




(defun select-trailing-source-files (after-file &key (all-files *init-files*))
  (let ((cur (reverse all-files))
	files file)
    (tagbody
     top
       (setq file (car cur))
       (if (endp cur) (go done))
       (if after-file
	   (if (eq after-file file)
	       (go done)))
       (if (not (keywordp file)) 
	   (setq files (cons file files)))
       (setq cur (cdr cur))
       (go top)
     done
       )
    files
    ))




(defun load-boot ( &optional last-file &key interp load-bitcode)
  (let* ((files (select-source-files last-file))
	 (cur files))
    (tagbody
     top
       (if (endp cur) (go done))
       (if (not interp)
	   (iload (car cur) :load-bitcode load-bitcode)
	   (interpreter-iload (car cur)))
       (setq cur (cdr cur))
       (go top)
     done
       )))


(defun compile-boot ( &optional last-file &key (recompile nil) )
  (load-boot last-file)
  (let* ((files (select-source-files last-file))
	 (cur files))
    (tagbody
     top
       (if (endp cur) (go done))
       (compile-iload (car cur) :recompile recompile)
       (setq cur (cdr cur))
       (go top)
     done
       )))

(defun recompile-boot (&optional last-file &key (load-bitcode t))
  (load-boot last-file)
  (let* ((files (select-source-files last-file))
	 (cur files))
    (tagbody
     top
       (if (endp cur) (go done))
       (let* ((filename (car cur))
	      (source-path (make-path (bformat nil "%s/CANDO/init/%s"
					       (as-string (script-dir)) filename)))
	      (bitcode-path (replace-extension (copy-path source-path) ".bc")))
	 (bformat t "Compiling %s\n" (path-file-name source-path))
	 (cmp:compile-file (as-string source-path) :output-file (as-string bitcode-path)))
       (setq cur (cdr cur))
       (go top)
     done
       )))





(defun clean-boot ( &optional after-file )
  (let* ((files (select-trailing-source-files after-file))
	 (cur files))
    (bformat t "Will remove modules: %s\n" files)
    (bformat t "cur=%s\n" cur)
    (tagbody
     top
       (if (endp cur) (go done))
       (delete-init-file (car cur) :really-delete t)
       (setq cur (cdr cur))
       (go top)
     done
       )))





(bformat t "Currently the (LOG x) macro is: %s\n" (macroexpand '(cmp:LOG x)))
;;(bformat t "And the cmp:*debug-compiler* variable is: %s\n" cmp:*debug-compiler*)
(bformat t "Compiler LOG macro will slow down compilation\n")
(bformat t "\n")
(bformat t "Useful commands:\n")
(bformat t "(load-boot &key :criterion :min :interp t )   - Load the minimal system\n")
(bformat t "(load-boot :nonclos)        - Load the system without clos\n")
(bformat t "(compile-boot)              - Compile whatever parts of the system have changed\n")
(bformat t "(recompile-boot &optional last-file &key interp load-bitcode)\n")
(bformat t "          - Recompile the system without loading it - do this after compile-boot\n")
(bformat t "          - Force loading of bitcode with :load-bitcode before recompile-boot to recompile new changes using the old compiler\n")
(bformat t "(recompile-boot)            - Recompile the entire system\n")
(bformat t "(clean-boot [after-file])   - Remove all boot files after after-file\n")
(bformat t "Available modules: %s\n" *init-files*)

