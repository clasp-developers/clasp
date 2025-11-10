(in-package #:cross-clasp)

(defclass native-compile-client () ())
(defvar *native-compile-client* (make-instance 'native-compile-client))

(defun module-native-compiler (bytecode literals-info pc-map module-id)
  (multiple-value-bind (irmodule fmap literals)
      (maclina->bir:compile-cmodule-into
       *native-compile-client* bytecode literals-info pc-map
       (make-instance 'cleavir-bir:module))
    (clasp-cleavir::bir-transformations irmodule clasp-cleavir:*clasp-system*)
    (let (;; KLUDGE
          (clasp-cleavir::*make-constant-info*
            #'maclina.compile::make-constant-info))
      (clasp-bytecode-to-bir::translate-cmodule
       irmodule fmap literals module-id
       (let ((f (m:symbol-value m:*client* *build-rte* '*compile-file-pathname*)))
         (if f
             (namestring f)
             "unknown-file"))))))

(defmethod maclina.compile-file::module-native-attr-name ((client client))
  "clasp:module-native")
(defmethod maclina.compile-file::function-native-attr-name ((client client))
  "clasp:function-native")

(in-package #:maclina.compile-file)

(defmethod ensure-module-literal ((info compiler::variable-cell-info))
  (ensure-vcell (compiler::variable-cell-info/vname info)))

(in-package #:clasp-bytecode-to-bir)

(defmethod maclina.compile-file::native-module-code ((mod nmodule))
  (nmodule-code mod))
(defmethod maclina.compile-file::native-module-fmap ((mod nmodule))
  (nmodule-fmap mod))
(defmethod maclina.compile-file::native-module-literals ((mod nmodule))
  (nmodule-literals mod))

(in-package #:clasp-cleavir)

(defmethod ensure-literal-info ((info maclina.compile:constant-info)
                                &optional (cinfo info))
  (ensure-similar (maclina.compile:constant-info-value info) t
      (vector-push-extend cinfo *constant-indices*)
      *similarity*))

(defmethod ensure-literal-info ((info maclina.compile:ltv-info) &optional (cinfo info))
  (let ((table (similarity-table-ltv *similarity*)))
    (or (gethash ltv-info table)
        (setf (gethash ltv-info table)
              (vector-push-extend cinfo *constant-indices*)))))

(defmethod ensure-literal-info ((vinfo maclina.compile:value-cell-info)
                                &optional (cinfo vinfo))
  (let ((table (similarity-table-vcell *similarity*))
        (vname (maclina.compile:value-cell-info-name vinfo)))
    (or (gethash vname table)
        (setf (gethash vname table)
              (vector-push-extend cinfo *constant-indices*)))))

(defmethod ensure-literal-info ((finfo maclina.compile:fdefinition-info)
                                &optional (cinfo finfo))
  (let ((table (similarity-table-fcell *similarity*))
        (fname (maclina.compile:fdefinition-info-name finfo)))
    (or (gethash fname table)
        (setf (gethash fname table)
              (vector-push-extend cinfo *constant-indices*)))))

(defmethod ensure-literal-info ((info maclina.compile:cfunction)
                                &optional (cinfo info))
  (let ((table (similarity-table-fungen *similarity*)))
    (or (gethash info table)
        (setf (gethash info table)
              (vector-push-extend cinfo *constant-indices*)))))
