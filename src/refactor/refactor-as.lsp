(getpid)


(progn
  (format t "---- 1. Setup and load $test-search asts~%")
  (require 'clang-tool)

  (load-compilation-database
   "/Users/meister/Development/clasp/build/clasp/Contents/Resources/build-databases/clasp_compile_commands.json")


  (defparameter $test-search (lsel $* ".*/numbers\.cc"))

  (defparameter *arg-adjuster*
    (lambda (args)
      (prog1
	  (concatenate 'vector #-quiet args #+quiet(remove "-v" args)
		       #("-DUSE_MPS"
			 "-DRUNNING_GC_BUILDER"
			 "-resource-dir"
			 "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/6.0"
			 )
		       (vector "-I/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.9.sdk/usr/include"
			       ))
	(format t "Leaving arg-adjuster~%")
	)))
  (load-asts $test-search :arguments-adjuster-code *arg-adjuster*)
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Define the ASTMatcher and the code to carry out the refactoring
;;;



(progn
  (format t "---- 2. Defining the matcher for class/field/method~%")
  (defparameter *matcher*
    '(:member-call-expr
      (:argument-count-is 0)
      (:bind :CALL
       (:member-call-expr))
      ))

  (format t "---- 3. Define some helper functions~%")
  (defparameter *refactor-fixnum-get*
    (make-instance
     'code-match-callback
     :match-code (lambda ()
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
			 (when (and (eql method-name "as") )
			   (format t "-------- loc: ~a~%" loc )
			   (format t "Call: ~a name: ~a~%" call method-name)
			   (format t "call-source: ~a~%" call-source)
			   (format t "expr: ~a~%" expr)
			   (format t "most-derived-type: ~a~%" most-derived-type)
			   (format t "m-d-t-name: ~a~%" m-d-t-name)
			   (format t "Source: ~a~%" expr-source)
			   (if (string/= call-source "")
			       (let ((type-start (position #\< call-source :from-end t))
				     (type-end (search "_O>" call-source :from-end t)))
				 (when type-end
				   (let* ((type (substr call-source (1+ type-start) (- type-end type-start 1)))
					  (replace-with (format nil "gc::As<~a_sp>(~a)" type expr-source)))
				     (format t "replace-with: ~a~%" replace-with)
				     (when *match-refactoring-tool*

				       (mtag-replace :CALL "~a" replace-with))
				     (format t "!!!!!Skipping replacement due to macro~%"))))))))))
     :end-of-translation-unit-code (lambda ()
				     (format t "!!!!!!!! Hit the end-of-translation-unit~%")
				     (format t "*match-refactoring-tool* ~a~%" *match-refactoring-tool*)
				     (format t "*run-and-save* ~a~%" *run-and-save*)
				     ))))
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Run and print info
;;;

;;;
;;; Test the matcher on the loaded asts
;;;
(progn
  (match-run *matcher* :the-code-match-callback *refactor-fixnum-get*)
  (print "Done"))

;;;
;;; Generate replacements but don't save them
;;;
(batch-match-run *matcher*
		 :filenames $test-search
		 :the-code-match-callback *refactor-fixnum-get*
		 :arguments-adjuster-code *arg-adjuster*)

;;;
;;; Generate replacements and write them back to the C++ code
;;; WARNING: No backups are kept - use git to rewind changes if they don't work
;;;
(batch-match-run *matcher*
		 :filenames $test-search
		 :the-code-match-callback *refactor-fixnum-get*
		 :arguments-adjuster-code *arg-adjuster*
		 :run-and-save t)
