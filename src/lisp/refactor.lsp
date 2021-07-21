(core:getpid)
(require :asdf)

(progn
  (asdf:load-asd (probe-file "source-dir:src;lisp;modules;clang-tool;clang-tool.asd"))
  (asdf:load-system :clang-tool))

(progn
  (format t "---- 1. Setup and load the compilation database~%")
  (defparameter *db* (clang-tool:load-compilation-tool-database
                      "/home/meister/Development/clasp/build/mpsprep/compile_commands.json")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Define the ASTMatcher and the code to carry out the refactoring
;;;



(progn
  (format t "---- 2. Defining the matcher for class/field/method~%")
;;;
;;; Adjust the compiler arguments for refactoring
;;;
  (defparameter *arg-adjuster* (lambda (args filename)
                                 (declare (ignore filename))
                                 (prog1
                                     (concatenate 'vector args
                                                  #("-DUSE_BOEHM"))
                                   (format t "Leaving arg-adjuster~%"))))

;;;
;;; Define the matcher for method calls with nullary (0) arguments
;;; Note: I could make this more specific to look for particular methods
;;;       but I'll do that checking in the callback
  (defparameter *matcher*
    '(:cxxmember-call-expr
      (:argument-count-is 0)
      (:bind :CALL
       (:cxxmember-call-expr))))

;;;
;;; Define the callback that looks for method calls like:
;;;    x->get() where x-> is dereferencing a Fixnum_O pointer
;;; rewrite it as unbox_fixnum(x)
;;;
  (defparameter *refactor-fixnum-get*
    (make-instance
     'clang-tool:code-match-callback
     :match-code
     (lambda (node)
       (let* ((call (clang-tool:mtag-node node :CALL))
	      (call-source (clang-tool:mtag-source node :CALL))
	      (loc (clang-tool:mtag-loc-start node :CALL))
	      (method-decl (cast:get-method-decl call)))
         (when method-decl
	   (let* ((method-name (cast:get-name-as-string method-decl))
		  (expr (cast:get-implicit-object-argument call))
		  (expr-source (clang-tool:mtag-source-impl expr))
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
		   (when clang-tool:*match-refactoring-tool*
		     (clang-tool:mtag-replace :CALL "unbox_fixnum(~a)" expr-source))
		   (format t "!!!!!Skipping replacement due to macro~%")))))))
     :end-of-translation-unit-code
     (lambda ()
       (format t "!!!!!!!! Hit the end-of-translation-unit~%")
       (format t "*match-refactoring-tool* ~a~%" clang-tool:*match-refactoring-tool*)
       (format t "*run-and-save* ~a~%" clang-tool:*run-and-save*)
       (let ((repl (ast-tooling:get-replacements clang-tool:*match-refactoring-tool*)))
         (format t "replacements: ~a~%" repl))))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Test and run 
;;;

;;; Testing
;;;
;;; Load a subset of the ASTs and run a quick test of the matcher on them
;;;
(defparameter *test-db* (clang-tool:copy-compilation-tool-database *db* :source-pattern "/array.cc"))

(defparameter *asts* (clang-tool:load-asts *test-db*))

(progn
  (clang-tool:match-run-loaded-asts *asts* *matcher* :callback *refactor-fixnum-get*)
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
