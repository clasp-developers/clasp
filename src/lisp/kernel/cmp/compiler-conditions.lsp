(in-package "CMP")

;;;; The final compiler condition system, replacing (and largely redefining)
;;;; the one in cmputil.lsp.
;;;; Much or most of this was originally cribbed from SBCL,
;;;; especially ir1report.lisp and main.lisp.

;;; If a condition has source info associated with it, return that.
;;; Otherwise NIL.
;;; This has methods defined on it for cleavir condition types later.
(defgeneric compiler-condition-origin (condition)
  (:method (condition) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Condition classes.

(define-condition compiler-condition (condition)
  ((%origin :reader compiler-condition-origin :initarg :origin)))

(define-condition undefined-variable-warning
    (warning compiler-condition)
  ((%name :reader compiler-warning-name :initarg :name))
  (:report (lambda (condition stream)
             (format stream "Undefined variable ~a"
                     (compiler-warning-name condition)))))

(define-condition undefined-function-warning
    (style-warning compiler-condition)
  ((%name :reader compiler-warning-name :initarg :name))
  (:report (lambda (condition stream)
             (format stream "Undefined function ~a"
                     (compiler-warning-name condition)))))

(define-condition redefined-function-warning
    (warning compiler-condition)
  ((%name :reader compiler-warning-name :initarg :name)
   (%old-type :reader redefinition-old-type :initarg :old-type)
   (%old-origin :reader redefinition-old-origin :initarg :old-origin)
   (%new-type :reader redefinition-new-type :initarg :new-type))
  (:report
   (lambda (condition stream)
     (let ((origin (redefinition-old-origin condition)))
       (format stream "The ~a ~a was previously defined as a ~a at ~a ~d:~d"
               (redefinition-new-type condition)
               (compiler-warning-name condition)
               (redefinition-old-type condition)
               (source-file-info-pathname
                (source-file-info origin))
               (source-pos-info-lineno origin)
               (source-pos-info-column origin))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Redefining some earlier error-noting calls, so that they
;;; actually signal.

(defun warn-redefined-function (name new-type new-origin
                                old-type old-origin)
  (warn 'redefined-function-warning
        :name name
        :old-type old-type
        :old-origin old-origin
        :new-type new-type
        :origin new-origin))

(defun warn-undefined-global-variable (origin name)
  (warn 'undefined-variable-warning
        :name name
        :origin origin))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Displaying conditions

(defgeneric print-compiler-condition (condition))

(defmethod print-compiler-condition :around (condition)
  (declare (ignore condition))
  (pprint-logical-block (*error-output* nil :per-line-prefix "; ")
    (call-next-method))
  ;; FIXME: Why is this necessary?
  (terpri *error-output*))

(defmethod print-compiler-condition :after (condition)
  (let ((origin (compiler-condition-origin condition)))
    (when origin
      (format *error-output* "~&    at ~a ~d:~d~%"
              (source-file-info-pathname
               (source-file-info origin))
              (source-pos-info-lineno origin)
              (source-pos-info-column origin)))))

(defun format-compiler-condition (what condition)
  (format *error-output* "caught ~S:~%~@<  ~@;~A~:>" what condition))

(defmethod print-compiler-condition ((condition error))
  (format-compiler-condition 'error condition))
(defmethod print-compiler-condition ((condition warning))
  (format-compiler-condition 'warning condition))
(defmethod print-compiler-condition ((condition style-warning))
  (format-compiler-condition 'style-warning condition))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Noting conditions in the compiler

(defvar *compiler-error-count*)
(defvar *compiler-warning-count*)
(defvar *compiler-style-warning-count*)

;;; Resignal the condition. If it's not handled, mark warningsp/failurep and print it.
;;; If possible, continue from an error, and for warnings just muffle (since we print)
(defun compiler-error-handler (condition)
  (signal condition)
  (incf *compiler-error-count*)
  (setf *warnings-p* t *failure-p* t)
  (print-compiler-condition condition)
  (continue condition))
(defun compiler-warning-handler (condition)
  (signal condition)
  (incf *compiler-warning-count*)
  (setf *warnings-p* t *failure-p* t)
  (print-compiler-condition condition)
  (muffle-warning condition))
(defun compiler-style-warning-handler (condition)
  (signal condition)
  (incf *compiler-style-warning-count*)
  (setf *warnings-p* t *failure-p* t)
  (print-compiler-condition condition)
  (muffle-warning condition))

;;; Redefinition. Used in e.g. compile-file.
(defun call-with-compilation-results (thunk &rest options)
  (declare (ignore options))
  (let ((*warnings-p* nil) (*failure-p* nil))
    (values
     (handler-bind
         ((error #'compiler-error-handler)
          (style-warning #'compiler-style-warning-handler)
          (warning #'compiler-warning-handler))
       (funcall thunk))
     *warnings-p* *failure-p*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compilation units

(defun do-compilation-unit (thunk &key override)
  (if (or (not *active-protection*) ; we're not in a compilation unit
          override) ; we are, but are overriding it
      (let ((*active-protection* t)
            (*global-function-defs* (make-global-function-defs-table))
            (*global-function-refs* (make-global-function-refs-table))
            (*compiler-error-count* 0)
            (*compiler-warning-count* 0)
            (*compiler-style-warning-count* 0)
            (abortedp t))
        (unwind-protect
             (multiple-value-prog1 (funcall thunk) (setf abortedp nil))
          (compilation-unit-finished abortedp)))
      (funcall thunk)))

(defun signal-undefinedness-warnings ()
  ;; Signal conditions for global function references that were never satisfied.
  ;; handler-bind is so the counts keep incrementing.
  (handler-bind ((style-warning #'compiler-style-warning-handler)
                 (warning #'compiler-warning-handler))
    (maphash (lambda (name references)
               (unless (or (fboundp name)
                           (gethash name *global-function-defs*))
                 (dolist (ref references)
                   (warn 'undefined-function-warning
                         :name name
                         :origin (global-function-ref-source-pos-info ref)))))
             *global-function-refs*)))

(defun compilation-unit-finished (abortedp)
  (signal-undefinedness-warnings)
  (unless (and (not abortedp)
               (zerop *compiler-error-count*)
               (zerop *compiler-warning-count*)
               (zerop *compiler-style-warning-count*))
    (pprint-logical-block (*error-output* nil :per-line-prefix "; ")
      (format *error-output* "~&compilation unit ~:[finished~;aborted~]"
              abortedp)
      (format *error-output* "~[~:;~:*~&  caught ~w ERROR condition~:P~]~
                            ~[~:;~:*~&  caught ~w WARNING condition~:P~]~
                            ~[~:;~:*~&  caught ~w STYLE-WARNING condition~:P~]"
              *compiler-error-count*
              *compiler-warning-count*
              *compiler-style-warning-count*))
    (terpri *error-output*)
    (force-output *error-output*)))
