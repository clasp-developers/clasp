(in-package #:compilation-environment)

;;; Say we have the toplevel form (locally (declare ...) (eval-when (:compile-toplevel) ...))
;;; The form in the eval-when will be evaluated in the load-environment, which should punt to
;;; the compilation environment. That is, (cleavir-env:eval ... entry load)
;;; But the environment has been constructed to have the load environment at the top again.
;;; So we need a new entry that has the compiler environment on top.

(defgeneric retarget (entry new))

(defmethod retarget (entry new)
  (declare (ignore entry))
  ;; We're on top, so we just return the new top and we're done.
  new)

(defmethod retarget ((entry cleavir-env::entry) new)
  (error "No RETARGET method defined for class ~a" (class-of entry)))

(macrolet ((defcopy (class &rest initargs-and-readers)
             `(defmethod retarget ((entry ,class) new)
                (make-instance ',class
                               ,@(loop for (initarg reader) on initargs-and-readers
                                       by #'cddr
                                       collect initarg
                                       collect `(,reader entry))
                               :next (retarget (cleavir-env::next entry) new)))))
  (defcopy cleavir-env:special-variable :name cleavir-env:name)
  (defcopy cleavir-env:symbol-macro :name cleavir-env:name :expansion cleavir-env:expansion)
  (defcopy cleavir-env:macro :name cleavir-env:name :expander cleavir-env:expander)
  (defcopy cleavir-env:variable-type :name cleavir-env:name :type cleavir-env:type)
  (defcopy cleavir-env:function-type :name cleavir-env:name :type cleavir-env:type)
  (defcopy cleavir-env:variable-ignore :name cleavir-env:name :ignore cleavir-env:ignore)
  (defcopy cleavir-env:function-ignore :name cleavir-env:name :ignore cleavir-env:ignore)
  (defcopy cleavir-env:variable-dynamic-extent :name cleavir-env:name)
  (defcopy cleavir-env:function-dynamic-extent :name cleavir-env:name)
  (defcopy cleavir-env:inline :name cleavir-env:name :inline cleavir-env:inline)
  ;; FIXME: export
  (defcopy cleavir-env::inline-expansion :name cleavir-env:name :ast cleavir-env:ast)
  (defcopy cleavir-env:optimize :optimize cleavir-env:optimize :policy cleavir-env:policy))
