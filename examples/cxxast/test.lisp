;;; -*- tabwidth:2

(progn
  (require 'clang-tool)
  (load-compilation-database
   "/Users/meister/Development/clasp/src/tests/lisp/georgey/compile_commands.json"
   :main-source-filename "test.cpp")
  (load-asts $*))


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



(defparameter *method-parm-matcher*
  (compile-matcher 
   '(:method-decl 
     (:for-each-descendant 
      (:bind :param 
       (:parm-var-decl))))))

(progn
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

(defun get-classes ()
  (let ((classes (make-hash-table :test 'equal)))
    (match-run *matcher*
     :code (lambda ()
	     (let* ((field-node (mtag-node-or-null :field))
		    (method-node (mtag-node-or-null :method))
		    (ast-name (mtag-name :whole))
		    (obj (or (gethash ast-name classes)
			     (setf (gethash ast-name classes)
				   (make-cxx-class :name ast-name)))))
	       (cond (method-node
		      (push (list (clang-ast:get-as-string
				   (mtag-node :ret-type))
				  (mtag-name :method)
				  (sub-match-collect
				   *method-parm-matcher*
				   method-node
				   (ast-node-typed-decl
				    (mtag-node :param))))
			    (cxx-class-methods obj)))
		     (field-node
		      (push (ast-node-typed-decl field-node)
			    (cxx-class-fields obj))))
                         )))
    classes))


(defparameter *classes* (get-classes))

(loop for v being the hash-values in *classes*
	 do (format t "#S(~A :NAME ~S~{~@[~&   :~A ~S~]~})~&"
		    (type-of v) (cxx-class-name v)
		    (list :fields (cxx-class-fields v)
			  :methods (cxx-class-methods v))))

#+(or)
(let ((classes (make-hash-table :test 'equal)))
  (match-run
   *matcher*
   :code (lambda ()
           (let* ((method-node (mtag-node-or-null :method))
                  (field-node (mtag-node-or-null :field))
                  (ast-name (mtag-name :whole))
                  (obj (or (gethash ast-name classes)
                           (setf (gethash ast-name classes)
                                 (make-cxx-class :name ast-name)))))
             (cond (method-node
                    (push (list (clang-ast:get-as-string
                                 (mtag-node :ret-type))
                                (mtag-name :method)
                                (sub-match-collect
                                 *method-parm-matcher*
                                 method-node
                                 (ast-node-typed-decl
                                  (mtag-node :param))))
                          (cxx-class-methods obj)))
                   (field-node
                    (push (ast-node-typed-decl field-node)
                          (cxx-class-fields obj))))
             )))
  classes)

