(in-package :cmp)



(defvar *generate-compile-file-load-time-values* nil
  "This variable controls whether literals are compiled into the
load-time-value manager (true - in COMPILE-FILE) or not (false - in COMPILE)."
)



;;; Contains the current RUN-ALL, initialization function
;;; for the current module
(defvar *run-all-function*)

(defvar *load-time-value-holder-global-var-type* nil
  "Store the current load-time-value data structure type for COMPILE-FILE")
(defvar *load-time-value-holder-global-var* nil
  "Store the current load-time-value data structure for COMPILE-FILE")

(defvar *irbuilder-run-all-alloca* nil
  "Maintains an IRBuilder for the load-time-value function alloca area")
(defvar *irbuilder-run-all-body* nil
  "Maintain an IRBuilder for the load-time-value body area")


(defun do-make-new-run-all (body name-suffix)
  (let ((run-all-fn (irc-simple-function-create (core:fmt nil "{}{}" core:+run-all-function-name+ name-suffix)
                                                    %fn-start-up%
                                                    'llvm-sys:internal-linkage
                                                    *the-module*
                                                    :argument-names +fn-start-up-argument-names+
                                                    :function-attributes (list* #|"optnone"|# *default-function-attributes*)
                                                    ))
        (irbuilder-alloca (llvm-sys:make-irbuilder (thread-local-llvm-context)))
        (irbuilder-body (llvm-sys:make-irbuilder (thread-local-llvm-context))))
    (let* ((*run-all-function* run-all-fn)
           (*irbuilder-run-all-alloca* irbuilder-alloca)
           (*irbuilder-run-all-body* irbuilder-body)
           (*current-function* run-all-fn))
      (cmp:with-guaranteed-*current-source-pos-info* ()
        (cmp:with-dbg-function (:lineno 0
                                :function run-all-fn
                                :function-type (cmp:fn-prototype :general-entry))
          ;; Set up dummy debug info for these irbuilders
          (let ((entry-bb (irc-basic-block-create "entry" run-all-fn)))
            (irc-set-insert-point-basic-block entry-bb irbuilder-alloca))
          (let ((body-bb (irc-basic-block-create "body" run-all-fn)))
            (irc-set-insert-point-basic-block body-bb irbuilder-body)
            ;; Setup exception handling and cleanup landing pad
            (with-irbuilder (irbuilder-alloca)
              (let ((entry-branch (irc-br body-bb)))
                (irc-set-insert-point-instruction entry-branch irbuilder-alloca)
                (with-irbuilder (irbuilder-body)
                  (funcall body run-all-fn)
                  (irc-ret-null-t*))))))))
    (values run-all-fn)))

(defmacro with-make-new-run-all ((run-all-fn &optional (name-suffix '(core:fmt nil "*{}" (core:next-number)))) &body body)
  "Set up a run-all function in the current module, return the run-all-fn"
  `(do-make-new-run-all (lambda (,run-all-fn)
                          (declare (ignorable ,run-all-fn))
                          (progn ,@body))
     ,name-suffix))

(defmacro with-run-all-entry-codegen (&body form)
  "Generate code within the ltv-function - used by codegen-load-time-value"
  `(let ((*irbuilder-function-alloca* *irbuilder-run-all-alloca*)
	 (*current-function* *run-all-function*))
     (cmp:with-landing-pad nil
       (cmp:with-irbuilder (*irbuilder-run-all-alloca*)
         ,@form))))

(defmacro with-run-all-body-codegen ( &body form)
  "Generate code within the ltv-function - used by codegen-load-time-value"
  `(let ((*irbuilder-function-alloca* *irbuilder-run-all-alloca*)
	 (*current-function* *run-all-function*))
     (cmp:with-landing-pad nil
       (cmp:with-irbuilder (*irbuilder-run-all-body*)
         ,@form))))

(defun generate-load-time-values () *generate-compile-file-load-time-values*)
