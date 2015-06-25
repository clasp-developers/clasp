;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This file contains code to compile cleavir and generate a new Clasp image
;;; in which Cleavir is the default compiler
;;;
(format t "Starting compile-cclasp.lisp~%")


(defpackage #:cclasp-build
  (:use #:common-lisp #:core))
(in-package :cclasp-build)

(defun compile-system (first-file last-file &key recompile reload (system *cleavir-system*))
;;  (if *target-backend* nil (error "*target-backend* is undefined"))
  (format t "compile-system  from: ~a  to: ~a\n" first-file last-file)
  (let* ((files (core:select-source-files last-file :first-file first-file :system system))
	 (cur files)
         bitcode-files)
    (tagbody
     top
       (if (endp cur) (go done))
       (let ((one-bitcode (core:compile-kernel-file (car cur) :recompile recompile :reload reload )))
         (setq bitcode-files (cons one-bitcode bitcode-files)))
       (setq cur (cdr cur))
       (go top)
     done)
    (reverse bitcode-files)))

(defun compile-clasp (from-mod to-mod &key (recompile t) reload (system *cleavir-system*) dry-run dont-link)
  (pushnew :clos *features*)
  (pushnew :cleavir *features*)
  (core:pathname-translations "cleavir-boehm" '(("**;*.*" #P"SYS:build;system;cleavir-boehm;**;*.*")))
  (core:pathname-translations "cleavir-mps" '(("**;*.*" #P"SYS:build;system;cleavir-mps;**;*.*")))
  (let* ((core:*target-backend* (core:default-target-backend))
	 (compiler-symbol (find-symbol "*COMPILER*" "CLEAVIR-GENERATE-AST")))
    (setf (symbol-value compiler-symbol) 'cl:compile-file)
    #+(or)(core:load-system :start :all :interp t )
    (if dry-run
	(format t "Compiling files: ~a~%" (core:select-source-files to-mod :first-file from-mod :system system))
	(let* ((cmp:*cleavir-compile-file-hook* (fdefinition (find-symbol "CLEAVIR-COMPILE-FILE-FORM" "CLASP-CLEAVIR")))
	       (bitcode-files (core:compile-system from-mod to-mod :recompile recompile :reload reload :system system)))
	  (unless dont-link 
	    (cmp:link-system-lto (core:target-backend-pathname core:+image-pathname+)
				 :lisp-bitcode-files bitcode-files
				 :prologue-form '(progn
						  (if (member :interactive *features*) 
						      (core:bformat t "Starting %s cclasp %s ... loading image... it takes a few seconds\n"
								    (if (member :use-mps *features*) "MPS" "Boehm" ) (software-version))))
				 :epilogue-form '(progn
						  (cl:in-package :cl-user)
						  (require 'system)
						  (core:load-clasprc)
						  (core:process-command-line-load-eval-sequence)
						  (when (member :interactive *features*) (core:top-level)))))))))

(defun compile-min-cclasp (&key (recompile t))
  (let ((cmp:*compile-print* t))
    (compile-clasp :start :min :recompile recompile :reload nil
				:system *cleavir-system*)))


(defun compile-full-cclasp (&key (recompile t) (reload nil) (system *cleavir-system*))
  (let ((cmp:*compile-print* t))
    (compile-clasp :init :auto-cleavir
                   :recompile recompile 
                   :reload reload
                   :system system)))

(export 'compile-full-cclasp)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Link files together
;;;

(defun bitcode-pathname (module &key (target-backend (core::default-target-backend)))
  (merge-pathnames (pathname (if (pathnamep module) module (string module)))
		   (make-pathname :host target-backend :directory '(:absolute) :type "bc")))


(defun select-bitcode-files (start end &key (target-backend (core::default-target-backend))
					 (system *cleavir-system*))
  (let ((rest (member start system)))
    (loop for mod in (member start system)
       until (eq mod end)
       unless (keywordp mod)
       collect (bitcode-pathname mod :target-backend target-backend))))


(defun link (start end &key (target-backend "CLEAVIR-BOEHM") (system *cleavir-system*))
  (let ((bitcode-files (select-bitcode-files start end :target-backend target-backend
					     :system system)))
    (cmp:link-system-lto (core::target-backend-pathname core::+image-pathname+ 
							:target-backend target-backend)
			 :lisp-bitcode-files bitcode-files
			 :prologue-form '(progn
					  (if (member :clos *features*) nil (setq *features* (cons :clos *features*)))
					  (if (member :interactive *features*) 
					      (core:bformat t "Starting %s Clasp %s ... loading image... it takes a few seconds\n" (if (member :use-mps *features*) "MPS" "Boehm" ) (software-version))))
			 :epilogue-form '(progn
					  (cl:in-package :cl-user)
					  (core::process-command-line-load-eval-sequence)
					  (when (member :interactive *features*) (core:run-repl)))
			 :target-backend target-backend)
    ))

(format t "Loading cleavir-system.lsp~%")
(load "sys:kernel;cleavir-system.lsp")

(core:load-system :bclasp :cclasp :system cclasp-build::*cleavir-system*)
