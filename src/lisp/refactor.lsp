(getpid)85165
(require 'clang-tool)

(load-compilation-database
 "/Users/meister/Development/clasp/build/clasp/Contents/Resources/build-databases/clasp_compile_commands.json")


(defparameter $test-search (lsel $* ".*/numbers\.cc"))


(load-asts $test-search
	   :arguments-adjuster-code
	   (lambda (args)
	     (concatenate 'vector #-quiet args #+quiet(remove "-v" args)
			  #("-DUSE_MPS"
			    "-DRUNNING_GC_BUILDER"
			    "-resource-dir"
			    "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/6.0"
			    )
			   (vector "-I/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.9.sdk/usr/include"
				   ))))





(progn
  (format t "---- 2. Defining the matcher for class/field/method~%")
  (defparameter *matcher*
    '(:member-call-expr
      (:argument-count-is 0)
      (:bind :CALL
       (:member-call-expr))
      )))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Define some helper functions
;;;


(progn
  (format t "---- 4. Define some helper functions~%")
  (defun get-rewrites ()
    (match-run
     *matcher*
     :code (lambda ()
	     (let* ((call (mtag-node :CALL))
		    (loc (mtag-loc-start :CALL))
		    (method-decl (cast:get-method-decl call))
		    (method-name (cast:get-name-as-string method-decl))
		    (expr (cast:get-implicit-object-argument call))
		    (expr-source (mtag-source-impl expr))
		    )
	       (when (eql method-name "get")
		 (format t "-------- loc: ~a~%" loc )
		 (format t "Call: ~a name: ~a~%" call method-name)
		 (format t "Source: ~a~%" expr-source)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Run and print info
;;;

(defparameter *classes* (get-rewrites))

(loop for v being the hash-values in *classes*
	 do (format t "#S(~A :NAME ~S~{~@[~&   :~A ~S~]~})~&"
		    (type-of v) (cxx-class-name v)
		    (list :fields (cxx-class-fields v)
			  :methods (cxx-class-methods v))))


(print *asts*)
