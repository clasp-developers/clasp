;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This file contains code to compile cleavir and generate a new Clasp image
;;; in which Cleavir is the default compiler
;;;

(defpackage #:cclasp-build
  (:use #:common-lisp #:core))
(in-package :cclasp-build)


(defun compile-system (first-file last-file &key recompile reload (system *cleavir-system*))
;;  (if *target-backend* nil (error "*target-backend* is undefined"))
  (format t "compile-system  from: ~a  to: ~a%N" first-file last-file)
  (let* ((files (core:select-source-files first-file last-file :system system))
	 (cur files)
         bitcode-files)
    (tagbody
     top
       (if (endp cur) (go done))
       (let ((one-bitcode (core:compile-kernel-file (car cur) :force-recompile recompile :reload reload )))
         (setq bitcode-files (cons one-bitcode bitcode-files)))
       (setq cur (cdr cur))
       (go top)
     done)
    (reverse bitcode-files)))

(defun compile-clasp (from-mod to-mod &key (recompile t) reload (system *cleavir-system*) dry-run dont-link)
  (pushnew :clos *features*)
  (pushnew :cclasp *features*)
p  (core:pathname-translations "cclasp-boehm" '(("**;*.*" #P"SYS:build;system;cclasp-boehm;**;*.*")))
  (core:pathname-translations "cclasp-mps" '(("**;*.*" #P"SYS:build;system;cclasp-mps;**;*.*")))
  (let* ((core:*target-backend* (core:default-target-backend))
	 (compiler-symbol (find-symbol "*COMPILER*" "CLEAVIR-GENERATE-AST")))
    (setf (symbol-value compiler-symbol) 'cl:compile-file)
    #+(or)(core:load-system :start :all :interp t )
    (if dry-run
	(format t "Compiling files: ~a~%" (core:select-source-files from-mod to-mod :system system))
	(let* ((cmp:*cleavir-compile-file-hook* (fdefinition (find-symbol "CLEAVIR-COMPILE-FILE-FORM" "CLASP-CLEAVIR")))
	       (bitcode-files (core:compile-system from-mod to-mod :force-recompile recompile :reload reload :system system)))
	  (unless dont-link
            (link-cclasp from-mod to-mod :system system))))))

(defun compile-min-cclasp (&key (recompile t))
  (let ((cmp:*compile-print* t))
    (compile-clasp :start :min :force-recompile recompile :reload nil
				:system *cleavir-system*)))


(defun compile-full-cclasp (system &key (recompile t) (reload nil))
  (let ((cmp:*compile-print* t))
    (compile-clasp :init :auto-cleavir
                   :force-recompile recompile 
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

(defun link-cclasp (from-mod to-mod &key (system *cleavir-system*))
  (let* ((bitcode-files (select-bitcode-files from-mod to-mod :system system)))
    (cmp:llvm-link (core:target-backend-pathname core:+image-pathname+)
                         :input-files bitcode-files
                         :prologue-form '(progn
                                          (make-package "CLEAVIR-AST")
                                          (make-package "CLASP-CLEAVIR-AST")
                                          (if (member :clos *features*) nil (setq *features* (cons :clos *features*)))
                                          (if (member :cclasp *features*) nil (setq *features* (cons :cclasp *features*)))
                                          (if (core:is-interactive-lisp)
                                              (core:bformat t "Starting %s cclasp %s ... loading image... it takes a few seconds%N"
                                                            (if (member :use-mps *features*) "MPS" "Boehm" ) (software-version))))
                         :epilogue-form '(progn
                                          (cl:in-package :cl-user)
                                          (core:maybe-load-clasprc)
                                          (core:process-command-line-load-eval-sequence)
                                          (let ((core:*use-interpreter-for-eval* nil))
                                            (when (core:is-interactive-lisp) (core:top-level)))))))
(export '(link-cclasp))
