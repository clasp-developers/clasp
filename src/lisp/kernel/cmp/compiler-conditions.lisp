(in-package "CMP")

;;;; The final compiler condition system, replacing (and largely redefining)
;;;; the one in cmputil.lisp.
;;;; Much or most of this was originally cribbed from SBCL,
;;;; especially ir1report.lisp and main.lisp.

;;; This variable can be bound to a source location; then any compiler
;;; conditions that don't have an origin attached can use this.
;;; This lets conditions be localized to at least a top level form,
;;; even if they're unexpected.
(defvar *default-condition-origin* nil)

;;; For later
(defgeneric deencapsulate-compiler-condition (condition)
  (:method ((condition condition)) condition))

;;; If a condition has source info associated with it, return that.
;;; Otherwise NIL.
;;; This has methods defined on it for cleavir condition types later.
(defgeneric compiler-condition-origin (condition)
  (:method ((condition condition)) nil)
  (:method :around ((condition condition))
    (or (call-next-method) *default-condition-origin*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Condition classes.

;;; This condition is signaled AT RUNTIME if the compiler (at COMPILE TIME)
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
   (%original-condition :reader original-condition :initarg :condition))
  (:report (lambda (condition stream)
             (format stream "Cannot evaluate form the compiler failed to compile.~@
                             Form:~%  ~a~@
                             Compile-time error:~%  ~a"
                     (compiled-program-error-form condition)
                     (original-condition condition)))))

;;; Abstract class.
(define-condition compiler-condition (condition)
  ((%origin :reader compiler-condition-origin :initarg :origin
            :initform nil)))

;;; Abstract
(define-condition undefined-warning (compiler-condition)
  ((%name :reader undefined-warning-name :initarg :name)
   (%kind :reader undefined-warning-kind :allocation :class))
  (:report (lambda (condition stream)
             (format stream "Undefined ~(~a~) ~s"
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

(define-condition wrong-argcount-warning
    (warning compiler-condition)
  ;; Slots match wrong-number-of-arguments in clos/conditions.lisp.
  ;; TODO: Shared superclass, shared accessor names (packages!)
  ((given-nargs :initarg :given-nargs :reader given-nargs)
   (min-nargs :initarg :min-nargs :reader min-nargs)
   ;; NIL means no maximum.
   (max-nargs :initarg :max-nargs :reader max-nargs))
  (:report (lambda (condition stream)
             (let* ((min (min-nargs condition)) (max (max-nargs condition)))
               (format stream "Function called with ~d arguments, but expected ~@?"
                       (given-nargs condition)
                       (cond ((null max) "at least ~d")
                             ((= min max) "exactly ~d")
                             ((zerop min) "at most ~*~d")
                             (t "between ~d and ~d"))
                       min max)))))

;; not my greatest name, i admit.
;; this condition is signaled when a compiler-macroexpander signals an error.
;; it's only a warning because we can ignore the compiler macro.
(define-condition compiler-macro-expansion-error-warning
    (warning compiler-condition)
  ((%original-condition :reader original-condition :initarg :condition))
  (:report (lambda (condition stream)
             (format stream "~a~%(Using original form instead.)"
                     (original-condition condition)))))

;; Redefined from primitive in bytecode_compiler.cc
;; to survive compiler macros signaling errors.
;; This is kind of a KLUDGE, and doesn't do all the nice encapsulation
;;  that we do in Cleavir. Plus we have no source location.
(defun cmp:expand-compiler-macro-safely (expander form env
                                         &optional (origin (ext:current-source-location)))
  (handler-case
      (funcall *macroexpand-hook* expander form env)
    (error (c)
      (warn 'compiler-macro-expansion-error-warning
            :condition c :origin origin)
      form)))

;; These conditions are signaled by the bytecode compiler.
;; They are NOT signaled by Cleavir which uses its own conditions,
;; so that's kind of ugly. FIXME.
(define-condition cmp:unused-variable (style-warning
                                       compiler-condition)
  ((%name :initarg :name :reader name)
   (%setp :initarg :setp :reader setp))
  (:report
   (lambda (condition stream)
     (let ((name (name condition)))
       (format stream "The ~:[function~;variable~] ~a is ~:[defined~;assigned~] but never used."
               (symbolp name)
               (if (symbolp name) name (second name))
               (setp condition))))))
(define-condition cmp:used-variable (style-warning
                                     compiler-condition)
  ((%name :initarg :name :reader name))
  (:report
   (lambda (condition stream)
     (let ((name (name condition)))
       (format stream "The ~:[function~;variable~] ~a is used, even though it was declared ~a."
               (symbolp name)
               (if (symbolp name) name (second name))
               'ignore)))))

(define-condition cmp:malformed-binding (program-error compiler-condition)
  ((%operator :initarg :operator :reader operator)
   (%binding :initarg :binding :reader binding))
  (:report
   (lambda (condition stream)
     (format stream "The ~s binding ~s is malformed."
             (operator condition) (binding condition)))))

;;; redefined from bytecode_compiler.cc.
(defun cmp:warn-unused-variable (name &optional (origin (ext:current-source-location)))
  (warn 'cmp:unused-variable :origin origin :name name :setp nil))
(defun cmp:warn-used-ignored-variable (name &optional (origin (ext:current-source-location)))
  (warn 'cmp:used-variable :origin origin :name name))
(defun cmp:warn-set-unused-variable (name &optional (origin (ext:current-source-location)))
  (warn 'cmp:unused-variable :origin origin :name name :setp t))
(defun cmp:malformed-binding (operator binding &optional (origin (ext:current-source-location)))
  (error 'cmp:malformed-binding :origin origin
                                :operator operator :binding binding))

;; This condition is signaled when an attempt at constant folding fails
;; due to the function being called signaling an error.
;; A full warning might be okay since this should correspond to a runtime
;; error, but SBCL signals a style warning and this is experimental, so it
;; will only be a style warning for now.
;; I can imagine some reasons, e.g. relating to the floating point env.
(define-condition fold-failure (style-warning compiler-condition)
  ((%operation :initarg :operation :reader fold-operation)
   (%operands :initarg :operands :reader fold-operands)
   (%original-condition :initarg :condition :reader original-condition))
  (:report (lambda (condition stream)
             (format stream "Lisp error during constant folding:~%~a
Operation was (~s~{ ~s~})."
                     (original-condition condition)
                     (fold-operation condition)
                     (fold-operands condition)))))

(define-condition simple-compiler-warning
    (simple-warning compiler-condition)
  ())

(define-condition simple-compiler-style-warning
    (core::simple-style-warning compiler-condition)
  ())

;;; These conditions are signaled when the compiler wants to tell the
;;; programmer something, but that something doesn't warrant even a style
;;; warning. For example, a note that the compiler cannot optimize something
;;; could be a note, since this inability is not a problem per se with the
;;; code, and it may just be that the compiler is inadequate.
(define-condition ext:compiler-note (compiler-condition)
  ())

(defun note (datum &rest arguments)
  ;; We don't use coerce-to-condition because there is no simple-note type
  ;; for cleanliness reasons.
  (let ((note (etypecase datum
                (ext:compiler-note datum)
                (symbol ; condition type
                 (apply #'make-condition datum arguments)))))
    (restart-case (signal note)
      (muffle-note ()
        :report "Silence note."
        (return-from note nil)))
    (format *error-output* "~&;;; Note: ~a~%" note)))

(defun muffle-note (&optional condition)
  ;; We use c-r-d instead of find-restart so that an error is signaled if
  ;; somehow the restart is not active (this would be a bug)
  (invoke-restart (si::coerce-restart-designator 'muffle-note condition)))

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

(defun warn-cannot-coerce (origin type)
  (warn 'simple-compiler-style-warning
        :origin origin
        :format-control "Cannot coerce to type ~s: unknown or not defined for coerce"
        :format-arguments (list type)))

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
    (if origin
        (let (;; deal with start/end pairs
              (origin (if (consp origin) (car origin) origin)))
          (handler-case
              (format *error-output* "~&    at ~a ~d:~d~%"
                      (file-scope-pathname
                       (file-scope origin))
                      (source-pos-info-lineno origin)
                      (source-pos-info-column origin))
            (error (e)
              ;; Recursive errors are annoying. Therefore,
              (format *error-output* "~&    at #<error printing origin ~a: ~a>~%"
                      origin e))))
        (format *error-output* "~&    at unknown location~%"))))

(defun format-compiler-condition (what condition)
  (format *error-output* "caught ~S:~%~@<  ~@;~A~:>" what condition))

(defmethod print-compiler-condition ((condition error))
  (format-compiler-condition 'error condition))
(defmethod print-compiler-condition ((condition warning))
  (format-compiler-condition 'warning condition))
(defmethod print-compiler-condition ((condition style-warning))
  (format-compiler-condition 'style-warning condition))
(defmethod print-compiler-condition ((condition ext:compiler-note))
  (format *error-output* "note:~%~@<  ~@;~a~:>" condition))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Noting conditions in the compiler

(defvar *compiler-error-count* 0)
(defvar *compiler-warning-count* 0)
(defvar *compiler-style-warning-count* 0)
(defvar *compiler-note-count* 0)

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
(defun compiler-note-handler (condition)
  (signal condition)
  (incf *compiler-note-count*)
  (print-compiler-condition condition)
  (muffle-note condition))

;;; Redefinition. Used in e.g. compile-file.
(defun call-with-compilation-results (thunk &rest options)
  (declare (ignore options))
  (let ((*warnings-p* nil) (*failure-p* nil))
    (values
     (handler-bind
         ((error #'compiler-error-handler)
          (style-warning #'compiler-style-warning-handler)
          (warning #'compiler-warning-handler)
          (ext:compiler-note #'compiler-note-handler))
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
            (*compiler-note-count* 0)
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
               (zerop *compiler-style-warning-count*)
               (zerop *compiler-note-count*))
    (pprint-logical-block (*error-output* nil :per-line-prefix "; ")
      (format *error-output* "~&compilation unit ~:[finished~;aborted~]"
              abortedp)
      (format *error-output* "~[~:;~:*~&  caught ~w ERROR condition~:P~]~
                              ~[~:;~:*~&  caught ~w WARNING condition~:P~]~
                              ~[~:;~:*~&  caught ~w STYLE-WARNING condition~:P~]
                              ~[~:;~:*~&  ~w note~:P~]"
              *compiler-error-count*
              *compiler-warning-count*
              *compiler-style-warning-count*
              *compiler-note-count*))
    (terpri *error-output*)
    (force-output *error-output*)))
