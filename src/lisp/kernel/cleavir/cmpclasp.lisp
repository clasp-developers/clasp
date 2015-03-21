(in-package :clasp-cleavir)




(defvar *cleavir-only-init-files*
  '(
    ;; Insert the cleavir source files here
    ))

(defvar *cleavir-init-files* (append (remove 'core:cmp/cmprepl core:*init-files*) *cleavir-only-init-files*))


(defun compile-system (first-file last-file &key recompile reload (system *init-files*))
;;  (if *target-backend* nil (error "*target-backend* is undefined"))
  (format t "compile-system  from: ~a  to: ~a\n" first-file last-file)
  (let* ((files (core:select-source-files last-file :first-file first-file :system system))
	 (cur files)
         bitcode-files)
    (tagbody
     top
       (if (endp cur) (go done))
       (let ((one-bitcode (core::compile-kernel-file (car cur) :recompile recompile :reload reload )))
         (setq bitcode-files (cons one-bitcode bitcode-files)))
       (setq cur (cdr cur))
       (go top)
     done
       )
    (reverse bitcode-files)))







(defun compile-clasp-with-cleavir (from-mod to-mod &key (recompile t) reload (system core:*init-files*))
  (when (not (member :cleavir *features*)) (push :cleavir *features*))
  (core:pathname-translations "cleavir-boehm" '(("**;*.*" #P"SYS:build;system;cleavir-boehm;**;*.*")))
  (core:pathname-translations "cleavir-mps" '(("**;*.*" #P"SYS:build;system;cleavir-mps;**;*.*")))
  (let* ((core:*target-backend* (core:default-target-backend))
	 (compiler-symbol (find-symbol "*COMPILER*" "CLEAVIR-GENERATE-AST")))
    (setf (symbol-value compiler-symbol) 'cl:compile-file)
    #+(or)(core:load-system :start :all :interp t )
    (let* ((cmp:*cleavir-compile-file-hook* (fdefinition (find-symbol "CLEAVIR-COMPILE-FILE-FORM" "CLASP-CLEAVIR")))
	   (bitcode-files (core:compile-system from-mod to-mod :recompile recompile :reload reload :system system)))
      (cmp:link-system-lto (core:target-backend-pathname core:+image-pathname+)
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

#|| (compile-clasp-with-cleavir 'core:lsp/format 'core:lsp/format) 
||#


(defun compile-min-cleavir (&key (recompile t))
  (let ((cmp:*compile-print* t))
    (compile-clasp-with-cleavir :start :min :recompile recompile :reload nil
				:system (remove 'core:cmp/cmprepl core:*init-files*))
    ))

(defun compile-full-cleavir (&key (recompile t))
  (let ((cmp:*compile-print* t))
    (compile-clasp-with-cleavir :start :all :recompile recompile :reload nil
				:system (remove 'core:cmp/cmprepl core:*init-files*))
    ))


(defun compile-cleavir (&key (recompile t))
  (with-open-file (clasp-cleavir:*debug-log* "/tmp/cleavir/all.log" :direction :output)
    (let ((*compile-print* t))
      (compile-min-cleavir :recompile recompile))))

(defun compile-cleavir-setf (&key (recompile t))
  (with-open-file (clasp-cleavir:*debug-log* "/tmp/cleavir/all.log" :direction :output)
    (let ((*compile-print* t))
      (compile-clasp-with-cleavir 'core:lsp/setf 'core:lsp/setf :recompile 
				  recompile :reload nil
				  :system (remove 'core:cmp/cmprepl core:*init-files*)))))

#||

(compile-clasp-with-cleavir :start :pre-cmp)
(print "Hello")

||#


