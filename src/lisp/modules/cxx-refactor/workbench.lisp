(load "sys:modules;clang-tool;clang-tool.lisp")

(progn
  (defun translate-include (args filename)
    "* Arguments
- args :: A vector of strings (compilation arguments)
* Description
Convert -Iinclude to -I<main-sourcefile-pathname>/include. Uses dynamic variable *main-directory-namestring*."
    (declare (ignore filename))
    (let ((main-directory-namestring (namestring (make-pathname :name nil :type nil :defaults (clang-tool:main-pathname)))))
      (dotimes (i (length args))
        (when (string= (elt args i) "-Iinclude")
          (setf (elt args i) (format nil "-I~a/include" main-directory-namestring))))
      args))

  (defun setup-db ()
    (let ((db (clang-tool:load-compilation-tool-database "source-dir:build;preciseprep;compile_commands.json" )))
      (setf (clang-tool:source-namestrings db) (list (find-if (lambda (x) (search "cons" x)) (clang-tool:source-namestrings db))))
      (push #'translate-include (clang-tool:arguments-adjuster-list db))
      db))
  (defparameter *db* (setup-db))
  (clang-tool:load-asts *db*))

(defparameter *matcher*    '(:call-expr 
                             (:bind :whole (:call-expr))
                             (:callee
                              (:function-decl
                               (:has-name "consp")))))

(defun dosearch()
  (clang-tool:with-compilation-tool-database *db*
    (clang-tool:match-run-loaded-asts
     *matcher*
     :limit 10
     :callback
     (make-instance
      'clang-tool:code-match-callback
      :match-code (lambda (match-info)
                    (let* ((node (clang-tool:mtag-node match-info :whole))
                           #+(or)(name (cast:get-qualified-name-as-string node))
                           (source-pos (clang-tool:mtag-loc-start match-info :whole))
                           (source-pos-end (clang-tool:mtag-loc-end match-info :whole)) )
                      (cast:dump node)
                      #+(or)(format t "Name: ~a~%" name)
                      (format t "Source: ~a ~a~%" source-pos source-pos-end )))))))


;;(dosearch)

#|
(defconstant +source-path+ "/Users/meister/Development/clasp/src/core/cons.cc")
(defconstant +source-path-length+ (length +source-path+))

(clang-tool:with-compilation-tool-database *db*
  (clang-tool:match-run-loaded-asts
   '(:member-call-expr
     (:has-argument 0
      (:expr
       (:has-descendant
        (:string-literal
         (:bind :arg0-string (:string-literal))))
       (:bind :arg0 (:expr))))
     (:has-argument 1
      (:expr
       #+(or)(:has-descendant
              (:expr
               (:bind :arg-string (:string-literal))))
       (:bind :arg1 (:expr))))
     (:callee
      (:decl
       (:bind :the-callee (:decl)))))
   :limit 100000
   :callback
   (make-instance
    'clang-tool:code-match-callback
    :match-code (lambda (match-info)
                  (when (string= (clang-tool:mtag-loc-start match-info :whole) +source-path+ :end1 +source-path-length+)
                    (let* ((node (clang-tool:mtag-node match-info :whole))
                           (source-pos (clang-tool:mtag-loc-start match-info :whole))
                           (string-node (clang-tool:mtag-node match-info :arg0-string)))
                      (cast:dump (clang-tool:mtag-node match-info :arg1))
                      (format t "String: ~a~%" (cast:get-string string-node))
                      (format t "Node name ~a~%" (clang-tool:mtag-name match-info :the-callee))
                      (format t "Source: ~a~%" source-pos)))))))
|#
