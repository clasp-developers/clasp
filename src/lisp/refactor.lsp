(getpid)


(progn
  (format t "---- 1. Setup and load the compilation database~%")
  (require 'clang-tool)

  (load-compilation-database
   "/Users/meister/Development/clasp/build/clasp/Contents/Resources/build-databases/clasp_compile_commands.json"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Define the ASTMatcher and the code to carry out the refactoring
;;;



(progn
  (format t "---- 2. Defining the matcher for class/field/method~%")

  ;;;
  ;;; Adjust the compiler arguments for refactoring
  ;;;
  (defparameter *arg-adjuster*
    (lambda (args)
      (prog1
	  (concatenate 'vector #-quiet args #+quiet(remove "-v" args)
		       #("-DUSE_MPS"
			 "-DRUNNING_GC_BUILDER"
			 "-resource-dir"
			 "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/6.0")
		       (vector "-I/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.9.sdk/usr/include"))
	(format t "Leaving arg-adjuster~%"))))

  ;;;
  ;;; Define the matcher for method calls with nullary (0) arguments
  ;;; Note: I could make this more specific to look for particular methods
  ;;;       but I'll do that checking in the callback
  (defparameter *matcher*
    '(:member-call-expr
      (:argument-count-is 0)
      (:bind :CALL
       (:member-call-expr))))

  ;;;
  ;;; Define the callback that looks for method calls like:
  ;;;    x->get() where x-> is dereferencing a Fixnum_O pointer
  ;;; rewrite it as unbox_fixnum(x)
  ;;;
  (defparameter *refactor-fixnum-get*
    (make-instance
     'code-match-callback
     :match-code
     (lambda ()
       (let* ((call (mtag-node :CALL))
	      (call-source (mtag-source :CALL))
	      (loc (mtag-loc-start :CALL))
	      (method-decl (cast:get-method-decl call)))
	 (when method-decl
	   (let* ((method-name (cast:get-name-as-string method-decl))
		  (expr (cast:get-implicit-object-argument call))
		  (expr-source (mtag-source-impl expr))
		  (most-derived-type (cast:get-best-dynamic-class-type expr))
		  (m-d-t-name (cast:get-qualified-name-as-string most-derived-type)))
	     (when (and (eql method-name "get") (string= m-d-t-name "core::Fixnum_O") )
	       (format t "-------- loc: ~a~%" loc )
	       (format t "Call: ~a name: ~a~%" call method-name)
	       (format t "call-source: ~a~%" call-source)
	       (format t "expr: ~a~%" expr)
	       (format t "most-derived-type: ~a~%" most-derived-type)
	       (format t "m-d-t-name: ~a~%" m-d-t-name)
	       (format t "Source: ~a~%" expr-source)
	       (if (string/= call-source "")
		   (when *match-refactoring-tool*
		     (mtag-replace :CALL "unbox_fixnum(~a)" expr-source))
		   (format t "!!!!!Skipping replacement due to macro~%")))))))
     :end-of-translation-unit-code
     (lambda ()
       (format t "!!!!!!!! Hit the end-of-translation-unit~%")
       (format t "*match-refactoring-tool* ~a~%" *match-refactoring-tool*)
       (format t "*run-and-save* ~a~%" *run-and-save*)
       (let ((repl (ast-tooling:get-replacements *match-refactoring-tool*)))
	 (format t "replacements: ~a~%" repl))))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Test and run 
;;;

;;; Testing
;;;
;;; Load a subset of the ASTs and run a quick test of the matcher on them
;;;
(defparameter $test-search (lsel $* ".*/array\.cc"))
(load-asts $test-search :arguments-adjuster-code *arg-adjuster*)

(progn
  (match-run *matcher* :the-code-match-callback *refactor-fixnum-get*)
  (print "Done"))


;;; Testing - although there is no feedback yet on the Replacements generated
;;;
;;; Generate replacements but don't save them
;;;
(batch-match-run *matcher*
		 :filenames $test-search
		 :the-code-match-callback *refactor-fixnum-get*
		 :arguments-adjuster-code *arg-adjuster*)

;;; Production run
;;;
;;; Generate replacements and write them back to the C++ code
;;; WARNING: No backups are kept - use git to rewind changes if they don't work
;;;
(batch-match-run *matcher*
		 :filenames $*
		 :the-code-match-callback *refactor-fixnum-get*
		 :arguments-adjuster-code *arg-adjuster*
		 :run-and-save t)
