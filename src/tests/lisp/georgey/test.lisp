;;; -*- tabwidth:2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Define the C++ files to analyze
;;; Parse and load their ASTs in *ASTS*
;;;
(progn
  (format t "---- 1. Parsing C++ source in test.cpp~%")
  (require 'clang-tool)
  (load-compilation-database
   "/Users/meister/Development/clasp/src/tests/lisp/georgey/compile_commands.json"
   :main-source-filename "test.cpp")
  (load-asts $*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Define the class/method/field matcher
;;;
(progn
  (format t "---- 2. Defining the matcher for class/field/method~%")
  (defparameter *matcher*
    '(:record-decl 
      (:for-each 
       (:any-of 
	(:bind :field 
	       (:field-decl
		(:unless 
		    (:any-of 
		     (:has-ancestor 
		      (:class-template-specialization-decl))
		     (:is-implicit)))))
	(:bind :method
	       (:method-decl
		(:unless (:is-implicit))
		(:returns (:bind :ret-type (:qual-type)))))))))
  ;; Matcher for function/method parameters
  (defparameter *method-parm-matcher*
    (compile-matcher 
     '(:method-decl 
       (:for-each-descendant 
	(:bind :param 
	 (:parm-var-decl)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Define some helper functions
;;;
(progn
  (format t "---- 3. Define some helper functions~%")
  (defun mtag-node-or-null (tag)
    (handler-case (mtag-node tag)
      (no-node-for-tag-error (err) nil)))

  (defun ast-node-typed-decl (node)
    (cons (clang-ast:get-as-string
	   (clang-ast:get-type node))
	  (clang-ast:get-name node)))

  (defmacro sub-match-collect (matcher node &body body)
    (let ((buf (gensym)))
      `(let ((,buf ())) (sub-match-run ,matcher ,node
				       (lambda () (push (progn ,@body) ,buf)))
	    ,buf)))

  (defstruct cxx-class name fields methods))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Define some helper functions
;;;
(progn
  (format t "---- 4. Define some helper functions~%")
  (defun get-classes ()
    (let ((classes (make-hash-table :test 'equal)))
      (match-run
       *matcher*
       :code (lambda ()
	       (let* ((field-node (mtag-node-or-null :field))
		      (method-node (mtag-node-or-null :method))
		      (ast-name (mtag-name :whole))
		      (obj (or (gethash ast-name classes)
			       (setf (gethash ast-name classes)
				     (make-cxx-class :name ast-name)))))
		 (or (format t "Found a match~%")
		     (and field-node  (format t " field-node: ~a~%" field-node))
		     (and method-node (format t "method-node: ~a~%" method-node)))
		 (cond (method-node
			(push (list (clang-ast:get-as-string
				     (mtag-node :ret-type))
				    (mtag-name :method)
				    (sub-match-collect
				     *method-parm-matcher*
				     method-node
				     (ast-node-typed-decl
				      (mtag-node :param))))
			      (cxx-class-methods obj))
			(format t "Matched parameters~%"))
		       (field-node
			(push (ast-node-typed-decl field-node)
			      (cxx-class-fields obj))))
		 )))
      classes)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Run and print info
;;;

(defparameter *classes* (get-classes))

(loop for v being the hash-values in *classes*
	 do (format t "#S(~A :NAME ~S~{~@[~&   :~A ~S~]~})~&"
		    (type-of v) (cxx-class-name v)
		    (list :fields (cxx-class-fields v)
			  :methods (cxx-class-methods v))))


(print *asts*)
