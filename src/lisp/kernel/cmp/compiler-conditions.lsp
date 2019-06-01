(in-package "CMP")

;;;; The final compiler condition system, replacing (and largely redefining)
;;;; the one in cmputil.lsp.
;;;; Much or most of this was originally cribbed from SBCL,
;;;; especially ir1report.lisp and main.lisp.

;;; For later
(defgeneric deencapsulate-compiler-condition (condition)
  (:method (condition) condition))

;;; If a condition has source info associated with it, return that.
;;; Otherwise NIL.
;;; This has methods defined on it for cleavir condition types later.
(defgeneric compiler-condition-origin (condition)
  (:method (condition) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Condition classes.

;;; This class is signaled AT RUNTIME if the compiler (at COMPILE TIME)
;;; signaled an error while compiling some form. So e.g.
;;; (compile nil '(lambda () (progv))) gets you a function, but if you
;;; call that function, this error is signaled.
;;; Right now (and probably indefinitely) only cclasp uses this.
;;; See comment in translate.lisp.
(define-condition compiled-program-error (program-error)
  (;; Despite the names, FORM and ORIGINAL-CONDITIONS are actually strings,
   ;; so that COMPILE-FILE can dump error forms properly.
   (%form :reader compiled-program-error-form :initarg :form)
   (%origin :reader compiled-program-error-origin :initarg :origin)
   (%original-condition :reader compiled-program-error-original-condition
                        :initarg :condition))
  (:report (lambda (condition stream)
             (format stream "Cannot evaluate form the compiler failed to compile.~@
                             Form:~%  ~a~@
                             Compile-time error:~%  ~a"
                     (compiled-program-error-form condition)
                     (compiled-program-error-original-condition condition)))))

;;; Abstract class.
(define-condition compiler-condition (condition)
  ((%origin :reader compiler-condition-origin :initarg :origin)))

;;; Abstract
(define-condition undefined-warning (compiler-condition)
  ((%name :reader undefined-warning-name :initarg :name)
   (%kind :reader undefined-warning-kind :allocation :class))
  (:report (lambda (condition stream)
             (format stream "Undefined ~(~a~) ~a"
                     (undefined-warning-kind condition)
                     (undefined-warning-name condition)))))

(define-condition undefined-variable-warning
    (warning undefined-warning)
  ((%kind :initform 'variable)))

(define-condition undefined-function-warning
    (style-warning undefined-warning)
  ((%kind :initform 'function)))

(define-condition undefined-type-warning
    (style-warning undefined-warning)
  ((%kind :initform 'type)))

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
               (file-scope-pathname
                (file-scope origin))
               (source-pos-info-lineno origin)
               (source-pos-info-column origin))))))

(define-condition simple-compiler-warning
    (simple-warning compiler-condition)
  ())

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

;;; This condition is signaled from compiler macros, and Cleavir will
;;; encapsulate it in a condition with better source info.
(defun warn-undefined-type (origin type)
  (warn 'undefined-type-warning
        :name type
        :origin origin))

(defun warn-invalid-number-type (origin type)
  (warn 'simple-compiler-warning
        :origin origin
        :format-control "Invalid number type: ~s"
        :format-arguments (list type)))

(defun warn-icsp-iesp-both-specified (origin)
  (warn 'simple-compiler-warning
        :origin origin
        :format-control ":initial-contents and :initial-element both specified"
        :format-arguments nil))

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
              (file-scope-pathname
               (file-scope origin))
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

;;; Let errors through - lower level code should handle them if able, and otherwise
;;; it's a compiler bug and we ought to enter the debugger.
;;; This does mean that even if a higher level handler handles the condition,
;;; compilation fails. FIXME?
;;; In sbcl this is bracketed in signal ... continue, but we have more specific things
;;; to do than continue. We could maybe use signal ... continue if lower level handlers
;;; resignaled (as they already do) but in a restart-case establishing a CONTINUE.
(defun compiler-error-handler (condition)
  (incf *compiler-error-count*)
  (setf *warnings-p* t *failure-p* t)
  (print-compiler-condition condition))
;;; Resignal the condition so higher level handlers can get at it. If it's not handled,
;;; mark warningsp/failurep and print the condition. Muffle the warning.
(defun compiler-warning-handler (condition)
  (signal condition)
  (incf *compiler-warning-count*)
  (setf *warnings-p* t *failure-p* t)
  (print-compiler-condition condition)
  (muffle-warning condition))
(defun compiler-style-warning-handler (condition)
  (signal condition)
  (incf *compiler-style-warning-count*)
  (setf *warnings-p* t)
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
