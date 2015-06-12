;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This file contains code to compile cleavir and generate a new Clasp image
;;; in which Cleavir is the default compiler
;;;


(in-package :clasp-cleavir)

(load "sys:kernel;cleavir;asdf-system-groveler.lisp")

;;; Determine the source files required by the :clasp-cleavir
;;; ASDF system
(defvar *cleavir-clasp-only* 
  (asdf-system-groveler:determine-complete-set-of-asdf-source-files 
   (list :clasp-cleavir)))

;;;
;;; Create a list of source files for clasp+cleavir
;;;   - Inject the kernel/cleavir/inlining.lisp file at :inlining
;;;   - Remove the cmprepl source file because it's not used by cleavir
;;;   - #P"/kernel/cleavir/auto-compile" sets up automatic compilation of top-level forms
(defun setup-clasp-cleavir-files (&optional (init-files core:*init-files*) (cleavir-files *cleavir-clasp-only*))
  ;; Inject the cleavir-injection.lisp source file at the :cleavir-injection point
  (let ((injection-point (member :cleavir-injection init-files)))
    (rplacd injection-point (cons #P"/kernel/cleavir/cleavir-injection" (cdr injection-point))))
  ;; Remove the cmprepl file and append the rest of the cleavir files
  (append (remove-if (lambda (p) (and (pathnamep p) (string-equal "cmprepl" (pathname-name p)))) init-files )
          (list :bclasp)
          *cleavir-clasp-only*
          (list :cleavir-clasp)
          (list #P"/kernel/cleavir/auto-compile")
          (list :auto-compile :cclasp)))

;;; Setup the files to build for cclasp
(defparameter *clasp-cleavir-files*
  (setup-clasp-cleavir-files core:*init-files* *cleavir-clasp-only*))

(defun save-all-files ()
  "Save the list of files in *clasp-cleavir-files* to #P\"sys:kernel;cleavir-system.lsp\""
  (with-open-file (fout "sys:kernel;cleavir-system.lsp" :direction :output)
    (print `(defparameter *cleavir-system* ',*clasp-cleavir-files*) fout)))


(defun compile-system (first-file last-file &key recompile reload (system *clasp-cleavir-files*))
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
     done
       )
    (reverse bitcode-files)))







(defun compile-clasp (from-mod to-mod &key (recompile t) reload (system *clasp-cleavir-files*) dry-run dont-link)
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

(defun compile-min-cleavir (&key (recompile t))
  (let ((cmp:*compile-print* t))
    (compile-clasp :start :min :recompile recompile :reload nil
				:system *clasp-cleavir-files*)))


(defun compile-full-cleavir (&key (recompile t) (system *clasp-cleavir-files*))
  (let ((cmp:*compile-print* t))
    (compile-clasp :init :auto-cleavir :recompile recompile :reload nil
				:system system)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Link files together
;;;

(defun bitcode-pathname (module &key (target-backend (core::default-target-backend)))
  (merge-pathnames (pathname (string module)) 
		   (make-pathname :host target-backend :directory '(:absolute) :type "bc")))


(defun select-bitcode-files (start end &key (target-backend (core::default-target-backend))
					 (system *clasp-cleavir-files*))
  (let ((rest (member start system)))
    (loop for mod in (member start system)
       until (eq mod end)
       unless (keywordp mod)
       collect (bitcode-pathname mod :target-backend target-backend))))


(defun link (start end &key (target-backend "CLEAVIR-BOEHM") (system *clasp-cleavir-files*))
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

