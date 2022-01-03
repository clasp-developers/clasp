(in-package :cmp)



(defvar *generate-compile-file-load-time-values* nil
  "This variable controls whether literals are compiled into the
load-time-value manager (true - in COMPILE-FILE) or not (false - in COMPILE)."
)



;;; Contains the current RUN-ALL, initialization function
;;; for the current module
(defvar *run-all-function*)
(define-symbol-macro %run-and-load-time-value-holder-global-var-type% cmp:%ltv*%) ;; Was +ltvsp*+
(defvar *run-time-values-table-name* "run_time_values_table")
(defvar *run-all-environment*)
;;;------
;;; Set up the run-time-values-table
;;;  set-run-time-values-table MUST be called to set the
;;;  global
#+(or)(defvar *run-time-values-table* (core:load-time-value-array *run-time-values-table-name* 0))
#+(or)(core:set-run-time-values-table *run-time-values-table-name*)

(defvar *load-time-value-holder-global-var* nil
  "Store the current load-time-value data structure for COMPILE-FILE")

#+(or)(defvar *run-time-values-table-global-var* nil
  "All load-time-values and quoted values are stored in this array accessed with an integer index"
  )


(defvar *irbuilder-run-all-alloca* nil
  "Maintains an IRBuilder for the load-time-value function alloca area")
(defvar *irbuilder-run-all-body* nil
  "Maintain an IRBuilder for the load-time-value body area")

(defvar *run-all-result*)


(defun do-make-new-run-all (body name-suffix)
  (let ((run-all-fn (irc-simple-function-create (core:bformat nil "%s%s" core:+run-all-function-name+ name-suffix)
                                                    %fn-start-up%
                                                    'llvm-sys:internal-linkage
                                                    *the-module*
                                                    :argument-names +fn-start-up-argument-names+
                                                    :function-attributes (list* "optnone" *default-function-attributes*)
                                                    ))
        (irbuilder-alloca (llvm-sys:make-irbuilder (thread-local-llvm-context)))
        (irbuilder-body (llvm-sys:make-irbuilder (thread-local-llvm-context))))
    (cmp-log "Setting special variables do-make-new-run-all%N")
    (let* ((*run-all-function* run-all-fn)
           (*irbuilder-run-all-alloca* irbuilder-alloca)
           (*irbuilder-run-all-body* irbuilder-body)
           (*current-function* run-all-fn))
      (cmp-log "Entering with-dbg-function%N")
      (cmp:with-guaranteed-*current-source-pos-info* ()
        (cmp:with-dbg-function (:lineno 0
                                :function run-all-fn
                                :function-type (cmp:fn-prototype :general-entry))
          ;; Set up dummy debug info for these irbuilders
          (let ((entry-bb (irc-basic-block-create "entry" run-all-fn)))
            (irc-set-insert-point-basic-block entry-bb irbuilder-alloca))
          (cmp-log "bb work do-make-new-run-all%N")
          (let ((body-bb (irc-basic-block-create "body" run-all-fn)))
            (irc-set-insert-point-basic-block body-bb irbuilder-body)
            ;; Setup exception handling and cleanup landing pad
            (with-irbuilder (irbuilder-alloca)
              (let ((entry-branch (irc-br body-bb)))
                (irc-set-insert-point-instruction entry-branch irbuilder-alloca)
                (with-irbuilder (irbuilder-body)
                  (progn
                    (cmp-log "running body do-make-new-run-all%N")
                    (funcall body run-all-fn name-suffix))
                  (irc-ret-null-t*))))))))
    (values run-all-fn)))

(defmacro with-make-new-run-all ((run-all-fn &optional (name-suffix '(core:bformat nil "*%d" (core:next-number)))) &body body)
  "Set up a run-all function in the current module, return the run-all-fn"
  `(do-make-new-run-all (lambda (,run-all-fn name-suffix)
                          (declare (ignorable ,run-all-fn name-suffix))
                          ;; for sure it was not meant to define name-suffix here
                          ;; but putting ,name-suffix is not right either
                          (progn
                            ,@body))
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

(defun ltv-global ()
  "called by cclasp"
  *load-time-value-holder-global-var*
  #+(or)(if cmp:*generate-compile-file-load-time-values*
      *load-time-value-holder-global-var*
      *run-time-values-table-global-var*))

(defun generate-load-time-values () *generate-compile-file-load-time-values*)

(defun module-literal-table ()
  (format t "literal::*gcroots-in-module* -> ~a~%" literal::*gcroots-in-module*)
  (format t "*load-time-value-holder-global-var* -> ~a~%" *load-time-value-holder-global-var*)
  #+(or)(format t "*run-time-values-table-global-var* -> ~a~%" *run-time-values-table-global-var*)
  #+(or)(if (generate-load-time-values)
      *load-time-value-holder-global-var*
      *run-time-values-table-global-var*))

