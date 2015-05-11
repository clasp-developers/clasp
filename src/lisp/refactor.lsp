(getpid)
(progn
  (format t "---- 1. Setup and load $test-search asts~%")
  (require 'clang-tool)

  (load-compilation-database
   "/Users/meister/Development/clasp/build/clasp/Contents/Resources/build-databases/clasp_compile_commands.json")


  (defparameter $test-search (lsel $* ".*/lispStream\.cc"))

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



(progn
  (format t "---- 2. Defining the matcher for class/field/method~%")
  (defparameter *matcher*
    '(:member-call-expr
      (:argument-count-is 0)
      (:bind :CALL
       (:member-call-expr))
      ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Define some helper functions
;;;


  (format t "---- 3. Define some helper functions~%")
  (defparameter *match-code*
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
		 (m-d-t-name (cast:get-qualified-name-as-string most-derived-type))
		 )
	    (when (and (eql method-name "get") (string= m-d-t-name "core::Fixnum_O") )
	      (format t "-------- loc: ~a~%" loc )
	      (format t "Call: ~a name: ~a~%" call method-name)
	      (format t "call-source: ~a~%" call-source)
	      (format t "expr: ~a~%" expr)
	      (format t "most-derived-type: ~a~%" most-derived-type)
	      (format t "m-d-t-name: ~a~%" m-d-t-name)
	      (format t "Source: ~a~%" expr-source)
	      (if (string/= call-source "")
		  (mtag-replace :CALL "unbox_fixnum(~a)" expr-source)
		  (format t "!!!!!Skipping replacement due to macro~%"))))))))
      
  (defun get-rewrites ()
    (match-run
     *matcher*
     :code *match-code* ))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Run and print info
;;;

;;; Run a test on the loaded asts
(progn
  (match-run *matcher* :code *match-code*)
  (print "Done"))

;;; Generate replacements but don't save them
(batch-match-run *matcher*
		 :filenames $*
		 :code *match-code*
		 :arguments-adjuster-code *arg-adjuster*)


;;; Generate replacements and save them
(batch-match-run *matcher*
		 :filenames $*
		 :code *match-code*
		 :arguments-adjuster-code *arg-adjuster*
		 :run-and-save t)

(ast-tooling:to-string (car *match-replacements*))


(dolist (r *match-replacements*)
  (print (ast-tooling:to-string r)))


(loop for v being the hash-values in *classes*
	 do (format t "#S(~A :NAME ~S~{~@[~&   :~A ~S~]~})~&"
		    (type-of v) (cxx-class-name v)
		    (list :fields (cxx-class-fields v)
			  :methods (cxx-class-methods v))))

(length *match-replacements*)

(defparameter *replace* (ast-tooling:deduplicate *match-replacements*))
(length *replace*)

(dolist (r *replace*)
  (print (ast-tooling:to-string r)))
(print *match-source-manager*)

$*

(print *asts*)
(time (core:sleep 1000))

(get-internal-real-time)
(print internal-time-units-per-second)
(time (core::sleep 50))
(/ 5 2)
(*)
