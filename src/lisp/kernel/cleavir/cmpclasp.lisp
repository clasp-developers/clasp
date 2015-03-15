
(require :asdf)

(asdf:load-system :clasp-cleavir)

(defun compile-clasp-with-cleavir (from-mod to-mod &key (recompile t) reload)
  (when (not (member :cleavir *features*)) (push :cleavir *features*))
  (core:pathname-translations "cleavir-boehm" '(("**;*.*" #P"SYS:build;system;cleavir-boehm;**;*.*")))
  (core:pathname-translations "cleavir-mps" '(("**;*.*" #P"SYS:build;system;cleavir-mps;**;*.*")))
  (let* ((core:*target-backend* (core:default-target-backend))
	 (compiler-symbol (find-symbol "*COMPILER*" "CLEAVIR-GENERATE-AST")))
    (setf (symbol-value compiler-symbol) 'cl:compile-file)
    #+(or)(core:load-system :start :all :interp t )
    (let* ((cmp:*cleavir-compile-file-hook* (fdefinition (find-symbol "CLEAVIR-COMPILE-FILE-FORM" "CLASP-CLEAVIR")))
	   (bitcode-files (core:compile-system from-mod to-mod :recompile recompile :reload reload)))
      (cmp:link-system-lto (core:target-backend-pathname +image-pathname+)
			   :lisp-bitcode-files bitcode-files
			   :prologue-form '(progn
					    (if (member :interactive *features*) 
						(core:bformat t "Starting %s cclasp %s ... loading image... it takes a few seconds\n" (if (member :use-mps *features*) "MPS" "Boehm" ) (software-version))))
			   :epilogue-form '(progn
					    (cl:in-package :cl-user)
					    (require 'system)
					    (core:load-clasprc)
					    (core:process-command-line-load-eval-sequence)
					    (when (member :interactive *features*) (core:top-level))))
      )))


(defun compile-min-cleavir ()
  (with-open-file (clasp-cleavir:*debug-log* "/tmp/cleavir/compilefile.log" :direction :output)
    (let ((cmp:*compile-print* t))
      (compile-clasp-with-cleavir :start :min :recompile t :reload nil)
      )))


#||

(compile-clasp-with-cleavir :start :pre-cmp)
(print "Hello")

||#


