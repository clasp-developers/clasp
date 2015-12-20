(require :clang-tool)

;;;
;;; Setup a compilation-tool-database
;;; It describes an entire hierarchy of source files
;;; that will be refactored all in one sweep.
;;;
(progn
  (defclass def ()
    ((exposed-name :initarg :exposed-name :accessor exposed-name)
     (method-name :initarg :method-name :accessor method-name)
     (source-location :initarg :source-location :accessor source-location)))
  (defparameter *verbose-callback* nil)
  (defun translate-include (args)
    "* Arguments
- args :: A vector of strings (compilation arguments)
* Description
Convert -Iinclude to -I<main-sourcefile-pathname>/include. Uses dynamic variable *main-directory-namestring*."
    (let ((main-directory-namestring (namestring (make-pathname :name nil :type nil :defaults (clang-tool:main-pathname)))))
      (dotimes (i (length args))
        (when (string= (elt args i) "-Iinclude")
          (setf (elt args i) (format nil "-I~a/include" main-directory-namestring))))
      args))
  (defun setup-db ()
    "* Description
Setup the compilation-tool-database."
    (let ((db (clang-tool:load-compilation-tool-database
               "app-resources:build-databases;cando_compile_commands.json"
               :convert-relative-includes-to-absolute t)))
      (push #'translate-include (clang-tool:arguments-adjuster-list db))
      db))
  (defparameter *db* (setup-db)))

;;;
;;; Setup the ASTMatcher to recognize .def("foo",&Class_O::foo)
;;;
(progn
  (defconstant +source-path+ (ext:getenv "CLASP_HOME"))
  (defconstant +source-path-length+ (length +source-path+))
  (defparameter *find-def-matcher*
    '(:member-call-expr
      (:bind :whole (:member-call-expr))
      (:has-argument 0
       (:expr
        (:has-descendant
         (:string-literal
          (:bind :arg0-string (:string-literal))))
        (:bind :arg0 (:expr))))
      (:has-argument 1
       (:unary-operator
        (:has-descendant
         (:decl-ref-expr
          (:has-declaration
           (:method-decl
            (:bind :arg1-decl (:method-decl))))))))
      (:callee
       (:decl
        (:bind :the-callee (:decl))))))
  (defparameter *defs* (make-hash-table :test #'equal))
  (defun find-def-initializer ()
    (clrhash *defs*))
  (defun find-def-callback (match-info)
    (when (string= (clang-tool:mtag-loc-start match-info :whole) +source-path+ :end1 +source-path-length+)
      (let* ((node (clang-tool:mtag-node match-info :whole))
             (source-pos (clang-tool:mtag-loc-start match-info :whole))
             (string-node (clang-tool:mtag-node match-info :arg0-string))
             (exposed-name (cast:get-string string-node))
             (arg1-decl (clang-tool:mtag-node match-info :arg1-decl))
             (method-name (cast:get-qualified-name-as-string arg1-decl)))
        (when *verbose-callback*
          (format t "exposed-name: ~a~%" exposed-name)
          (format t "arg1-decl: ~a~%" (cast:get-qualified-name-as-string arg1-decl)))
        (setf (gethash method-name *defs*)  (make-instance 'def
                                                           :exposed-name exposed-name
                                                           :method-name method-name
                                                           :source-location source-pos))))))

;;; Load a subset of the ASTs for quick testing.
(progn
  (setf (clang-tool:source-namestrings *db*) (clang-tool:select-source-namestrings *db* ".*str\.cc.*"))
  (clang-tool:load-asts *db*))
;;; Run the matcher on the subset of ASTs
(progn
  (clang-tool:with-compilation-tool-database *db*
    (let ((*verbose-callback* t))
      (funcall #'find-def-initializer)
      (clang-tool:match-run-loaded-asts
       *find-def-matcher*
       :limit 100000
       :callback
       (make-instance
        'clang-tool:code-match-callback
        :match-code #'find-def-callback)))))

;;; Select just one of the source files for testing
(setf (clang-tool:source-namestrings *db*)
      (clang-tool:select-source-namestrings *db* ".*str\.cc.*$"))

;;; Select all of the source files
(setf (clang-tool:source-namestrings *db*)
      (clang-tool:select-source-namestrings *db*))

;;; Setup and run the multitool on the compilation-tool-database
;;; This will identify all of the class_<Foo_O>(...).def("bar",&Foo_O::bar);
(progn
  (defparameter *tool* (clang-tool:make-multitool))
  (clang-tool:multitool-add-matcher
   *tool*
   :name :find-defs
   :matcher (clang-tool:compile-matcher *find-def-matcher*)
   :initializer #'find-def-initializer
   :callback (make-instance 'clang-tool:code-match-callback
                            :match-code #'find-def-callback))
  (clang-tool:with-compilation-tool-database *db*
    (let ((*verbose-callback* t))
      (clang-tool:batch-run-multitool *tool*
                                      :compilation-tool-database *db*
                                      :run-and-save nil)))
  (format t "Done stage1~%"))


(defparameter *fix-method-matcher*
  '(:method-decl
    (:is-definition)
    (:bind :whole (:method-decl))))
(defparameter *fixed* nil)
(defun fix-method-initializer () (setf *fixed* (make-hash-table :test #'equal)))
(defun fix-method-callback (match-info)
  (when (string= (clang-tool:mtag-loc-start match-info :whole)
                 +source-path+ :end1 +source-path-length+)
    (let* ((node (clang-tool:mtag-node match-info :whole))
           (source-pos (clang-tool:mtag-loc-start match-info :whole))
           (method-name (cast:get-qualified-name-as-string node))
           (def-info (gethash method-name *defs*))
           (fixed-already (gethash method-name *fixed*)))
      (when (and *verbose-callback*
                 def-info
                 (not fixed-already))
        (format t "Method is exposed: ~a~%" method-name))
      (when (and def-info (not fixed-already))
        (setf (gethash method-name *fixed*) t)
        (clang-tool:mtag-replace
         match-info
         :whole
         (lambda (minfo tag)
           (let ((source (clang-tool:mtag-source minfo tag)))
             (format nil "CL_NAME(~s);~%CL_DEFMETHOD ~a" (exposed-name def-info) source))))))))

(progn
  (defparameter *tool* (clang-tool:make-multitool))
  (clang-tool:multitool-add-matcher
   *tool*
   :name :fix-expose
   :matcher (clang-tool:compile-matcher *fix-method-matcher*)
   :initializer #'fix-method-initializer
   :callback (make-instance 'clang-tool:code-match-callback
                            :match-code #'fix-method-callback))
  (clang-tool:with-compilation-tool-database *db*
    (let ((*verbose-callback* t))
      (clang-tool:batch-run-multitool *tool*
                                      :compilation-tool-database *db*
                                      :run-and-save nil)))
  (format t "Done stage2~%"))

