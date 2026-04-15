(in-package :clasp-cleavir)

;;; FIXME: Move this earlier
;; changed by de/proclaim
(defvar *ftypes* (make-hash-table :test #'equal))

(defun global-ftype (name)
  (multiple-value-bind (value presentp) (gethash name *ftypes*)
    (if presentp
        value
        (load-time-value (cleavir-ctype:function-top *clasp-system*)))))

(defun (setf global-ftype) (type name)
  (setf (gethash name *ftypes*)
        (env:parse-type-specifier type *clasp-env* *clasp-system*)))

(defvar *vtypes* (make-hash-table :test #'eq))
(defun global-type (name)
  (multiple-value-bind (value presentp) (gethash name *vtypes*)
    (if presentp
        value
        (load-time-value (cleavir-ctype:top *clasp-system*)))))
(defun (setf global-type) (type name)
  (setf (gethash name *vtypes*)
        (env:parse-type-specifier type *clasp-env* *clasp-system*)))

(defvar *fn-flags* (make-hash-table :test #'equal))
(defvar *fn-transforms* (make-hash-table :test #'equal))
(defvar *derivers* (make-hash-table :test #'equal))
(defvar *folds* (make-hash-table :test #'equal))

(macrolet ((define-function-flags (name &rest attributes)
             `(setf (gethash ',name *fn-flags*)
                    (cleavir-attributes:make-flags ,@attributes))))
  ;; FIXME: Can't do DX-call for many things like APPLY, FUNCALL, etc.
  ;; because we don't distinguish between *which* functional argument
  ;; is DX.
  (define-function-flags apply :dyn-call)
  (define-function-flags funcall :dyn-call)
  (define-function-flags every :dyn-call :dx-call)
  (define-function-flags some :dyn-call :dx-call)
  (define-function-flags notevery :dyn-call :dx-call)
  (define-function-flags notany :dyn-call :dx-call)
  (define-function-flags sublis :dyn-call :dx-call)
  (define-function-flags nsublis :dyn-call :dx-call)
  (define-function-flags subst-if :dyn-call :dx-call)
  (define-function-flags subst-if-not :dyn-call :dx-call)
  (define-function-flags nsubst-if :dyn-call :dx-call)
  (define-function-flags nsubst-if-not :dyn-call :dx-call)
  (define-function-flags member :dyn-call :dx-call)
  (define-function-flags member-if :dyn-call :dx-call)
  (define-function-flags member-if-not :dyn-call :dx-call)
  (define-function-flags mapc :dyn-call :dx-call)
  (define-function-flags mapcar :dyn-call :dx-call)
  (define-function-flags mapcan :dyn-call :dx-call)
  (define-function-flags mapl :dyn-call :dx-call)
  (define-function-flags maplist :dyn-call :dx-call)
  (define-function-flags mapcon :dyn-call :dx-call)
  (define-function-flags assoc :dyn-call :dx-call)
  (define-function-flags assoc-if :dyn-call :dx-call)
  (define-function-flags assoc-if-not :dyn-call :dx-call)
  (define-function-flags rassoc :dyn-call :dx-call)
  (define-function-flags rassoc-if :dyn-call :dx-call)
  (define-function-flags rassoc-if-not :dyn-call :dx-call)
  (define-function-flags intersection :dyn-call :dx-call)
  (define-function-flags nintersection :dyn-call :dx-call)
  (define-function-flags adjoin :dyn-call :dx-call)
  (define-function-flags set-difference :dyn-call :dx-call)
  (define-function-flags nset-difference :dyn-call :dx-call)
  (define-function-flags set-exclusive-or :dyn-call :dx-call)
  (define-function-flags nset-exclusive-or :dyn-call :dx-call)
  (define-function-flags subsetp :dyn-call :dx-call)
  (define-function-flags union :dyn-call :dx-call)
  (define-function-flags nunion :dyn-call :dx-call)
  (define-function-flags map :dyn-call :dx-call)
  (define-function-flags map-into :dyn-call :dx-call)
  (define-function-flags merge :dyn-call :dx-call)
  (define-function-flags bir:map-iblocks :dyn-call :dx-call)
  (define-function-flags bir:map-iblock-instructions :dyn-call :dx-call)
  ;; Can't do DYN-CALL for most sequence functions, as ext sequence
  ;; functions can do arbitrary things.
  (define-function-flags core:progv-function :dx-call)
  (define-function-flags core:funwind-protect :dx-call)
  (define-function-flags maphash :dx-call)
  (define-function-flags remove :dx-call)
  (define-function-flags remove-if :dx-call)
  (define-function-flags remove-if-not :dx-call)
  (define-function-flags delete :dx-call)
  (define-function-flags delete-if :dx-call)
  (define-function-flags delete-if-not :dx-call)
  (define-function-flags reduce :dx-call)
  (define-function-flags remove-duplicates :dx-call)
  (define-function-flags delete-duplicates :dx-call)
  (define-function-flags substitute :dx-call)
  (define-function-flags substitute-if :dx-call)
  (define-function-flags substitute-if-not :dx-call)
  (define-function-flags nsubstitute :dx-call)
  (define-function-flags nsubstitute-if :dx-call)
  (define-function-flags nsubstitute-if-not :dx-call)
  (define-function-flags count :dx-call)
  (define-function-flags count-if :dx-call)
  (define-function-flags count-if-not :dx-call)
  (define-function-flags find :dx-call)
  (define-function-flags find-if :dx-call)
  (define-function-flags find-if-not :dx-call)
  (define-function-flags position :dx-call)
  (define-function-flags position-if :dx-call)
  (define-function-flags position-if-not :dx-call)
  (define-function-flags mismatch :dx-call)
  (define-function-flags search :dx-call)
  (define-function-flags sort :dx-call)
  (define-function-flags stable-sort :dx-call)

  (define-function-flags core::map-into-sequence :dyn-call :dx-call)
  (define-function-flags core::map-into-sequence/1 :dyn-call :dx-call)
  (define-function-flags core::map-for-effect :dyn-call :dx-call)
  (define-function-flags core::map-for-effect/1 :dyn-call :dx-call)
  (define-function-flags core::map-to-list :dyn-call :dx-call)
  (define-function-flags core::map-to-list/1 :dyn-call :dx-call)
  (define-function-flags core::every/1 :dyn-call :dx-call)
  (define-function-flags core::some/1 :dyn-call :dx-call))

(defun function-attributes (function-name)
  (let* ((flags (gethash function-name *fn-flags*))
         (transforms (gethash function-name *fn-transforms*))
         (derivers (gethash function-name *derivers*))
         (folds (gethash function-name *folds*))
         (vaslistablep (cc-vaslist:vaslistablep function-name)))
    (if (or flags transforms folds derivers)
        (make-instance 'cleavir-attributes:attributes
          :flags (or flags (cleavir-attributes:make-flags))
          :identities (if (or transforms folds
                              derivers vaslistablep)
                          (list function-name)
                          nil))
        (cleavir-attributes:default-attributes))))

(defun global-inline-status (name)
  "Return 'cl:inline 'cl:notinline or nil"
  (cond
    ((core:declared-global-inline-p name) 'cl:inline)
    ((core:declared-global-notinline-p name) 'cl:notinline)
    (t nil)))

(setf cmp:*policy*
     (policy:compute-policy *clasp-system* cmp:*optimize*))

(defmethod env:optimize-info ((sys clasp) (environment clasp-global-environment))
  ;; The default values are all 3.
  (make-instance 'env:optimize-info
    :optimize cmp:*optimize*
    :policy cmp:*policy*))

(defmethod env:optimize-info ((sys clasp) (environment NULL))
  (env:optimize-info sys *clasp-env*))

(defmethod env:optimize-info ((sys clasp) (env cmp:lexenv))
  ;; FIXME: We will probably need lexenvs to track this eventually
  (env:optimize-info sys *clasp-env*))

(defun type-expand-1 (type-specifier &optional env)
  (let (head)
    (etypecase type-specifier
      (class (return-from type-expand-1 (values type-specifier nil)))
      (symbol (setf head type-specifier))
      (cons (setf head (first type-specifier))))
    (let ((def (ext:type-expander head)))
      (if def
          (values (funcall def type-specifier env) t)
          (values type-specifier nil)))))

(defmethod env:type-expand ((sys clasp) (environment clasp-global-environment)
                            type-specifier)
  ;; BEWARE: bclasp is really bad at unwinding, and mvb creates a
  ;; lambda, so we write this loop in a way that avoids RETURN. cclasp
  ;; will contify this and produce more efficient code anyway.
  (labels ((expand (type-specifier ever-expanded)
             (multiple-value-bind (expansion expanded)
                 (type-expand-1 type-specifier environment)
               (if expanded
                   (expand expansion t)
                   (values type-specifier ever-expanded)))))
    (expand type-specifier nil)))

(defmethod env:type-expand ((sys clasp) (environment null) type-specifier)
  (env:type-expand sys clasp-cleavir:*clasp-env* type-specifier))

;;; Needed because the default method ends up with classes,
;;; and that causes bootstrapping issues.
(defmethod env:find-class (name environment (system clasp) &optional errorp)
  (declare (ignore environment errorp))
  name)

;;; Used to pull out of CSTs, but we're no longer doing that.
;;; Kept in case we need something like that later.
(defun origin-source (origin)
  origin)

(defmethod cmp:compiler-condition-origin
    ((condition cleavir-conditions:program-condition))
  ;; FIXME: ignore-errors is a bit paranoid
  (let ((source (origin-source (cleavir-conditions:origin condition))))
    (ignore-errors (if (consp source) (car source) source))))

(in-package #:core)

;;; FCGE support functions, used by e.g. bytecode interpreter
(defgeneric fcge-ensure-fcell (environment name))
(defgeneric fcge-ensure-vcell (environment name))

(defgeneric fcge-find-package (environment name))
(defgeneric fcge-package-name (environment name))

;; Done through clostrum methods, but only in cross-clasp at the moment.
;; FIXME
(defgeneric fcge-lookup-fun (environment name))
(defgeneric fcge-lookup-var (environment name))

;;; These methods are not actually necessary since the runtime treats NIL
;;; environments specially, but they're here for completeness.
(defmethod fcge-ensure-fcell ((env null) name) (ensure-function-cell name))
(defmethod fcge-ensure-vcell ((env null) name) (ensure-variable-cell name))
(defmethod fcge-find-package ((env null) name) (find-package name))
(defmethod fcge-package-name ((env null) package) (package-name package))
