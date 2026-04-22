(in-package #:core)

(defvar *command-line-extensions* (make-hash-table :test 'equal)
  "Maps option-string (e.g. \"--swank\" or \"--ext:swank\") to a plist
of (:takes-arg BOOL :handler FUNCTION :help STRING).")

(defun register-command-line-option (option &key takes-arg handler help)
  "Register an extension command-line option.
OPTION is the literal flag string, e.g. \"--swank\" or \"--ext:swank-port\".
If TAKES-ARG is true, HANDLER is called with the next token as a string.
Otherwise HANDLER is called with no arguments.
HELP is a one-line description shown by ext:print-extension-command-line-help."
  (check-type option string)
  (check-type handler (or symbol function))
  (setf (gethash option *command-line-extensions*)
        (list :takes-arg takes-arg :handler handler :help help))
  option)

(defun unregister-command-line-option (option)
  (remhash option *command-line-extensions*))

(defun print-extension-command-line-help (&optional (stream *standard-output*))
  (maphash (lambda (option spec)
             (format stream "  ~a~:[~; <arg>~]~%      ~a~%"
                     option (getf spec :takes-arg) (or (getf spec :help) "")))
           *command-line-extensions*))

(defun %parse-ext-equals-form (arg)
  "If ARG is of the form --ext:NAME=VALUE, return (values NAME VALUE).
Otherwise return NIL."
  (when (and (>= (length arg) 7)
             (string= arg "--ext:" :end1 6))
    (let ((eq (position #\= arg :start 6)))
      (when eq
        (values (subseq arg 0 eq) (subseq arg (1+ eq)))))))

(defun process-extension-command-line-arguments ()
  "Walk core:extension-command-line-arguments, dispatching each registered
option to its handler. Errors on any unclaimed token."
  (let ((args (extension-command-line-arguments)))
    (loop while args
          for arg = (pop args)
          do (multiple-value-bind (eq-name eq-value) (%parse-ext-equals-form arg)
               (let* ((lookup (or eq-name arg))
                      (spec (gethash lookup *command-line-extensions*)))
                 (cond
                   ((null spec)
                    (error "Unrecognized command-line option: ~a" arg))
                   (eq-name
                    (funcall (getf spec :handler) eq-value))
                   ((getf spec :takes-arg)
                    (unless args
                      (error "Command-line option ~a requires an argument" arg))
                    (let ((value (pop args)))
                      (when (gethash value *command-line-extensions*)
                        (error "Command-line option ~a expected a value but got another option ~a"
                               arg value))
                      (funcall (getf spec :handler) value)))
                   (t
                    (funcall (getf spec :handler)))))))))

(export '(register-command-line-option
          unregister-command-line-option
          print-extension-command-line-help)
        :ext)
