(in-package :cmp)



(defvar *generate-compile-file-load-time-values* nil
  "This variable controls whether literals are compiled into the
load-time-value manager (true - in COMPILE-FILE) or not (false - in COMPILE)."
)



;;; Contains the current RUN-ALL, initialization function
;;; for the current module
(defvar *run-all-function*)
(defvar +run-and-load-time-value-holder-global-var-type+ cmp:+ltv*+) ;; Was +ltvsp*+
(defvar *run-time-values-table-name* "run_time_values_table")
(defvar *run-all-environment*)
;;;------
;;; Set up the run-time-values-table
;;;  set-run-time-values-table MUST be called to set the
;;;  global
(defvar *run-time-values-table* (core:load-time-value-array *run-time-values-table-name* 0))
(core:set-run-time-values-table *run-time-values-table-name*)

(defvar *load-time-value-holder-global-var* nil
  "Store the current load-time-value data structure for COMPILE-FILE")

(defvar *run-time-values-table-global-var* nil
  "All load-time-values and quoted values are stored in this array accessed with an integer index"
  )


(defvar *irbuilder-run-all-alloca* nil
  "Maintains an IRBuilder for the load-time-value function alloca area")
(defvar *irbuilder-run-all-body* nil
  "Maintain an IRBuilder for the load-time-value body area")

(defvar *run-all-result*)

(defmacro with-make-new-run-all ((run-all-fn) &body body)
  "Set up a run-all function in the current module, return the run-all-fn"
  (let ((cleanup-block-gs (gensym "ltv-cleanup-block"))
	(irbuilder-alloca (gensym "ltv-irbuilder-alloca"))
	(irbuilder-body (gensym "ltv-irbuilder-body"))
	(traceid-gs (gensym "traceid"))
	(fn-env-gs (gensym "ltv-fn-env"))
        (result (gensym "result")))
    `(multiple-value-bind (,run-all-fn ,fn-env-gs ,cleanup-block-gs
					,irbuilder-alloca ,irbuilder-body ,result )
	 (irc-bclasp-function-create core:+run-all-function-name+
                                     nil nil
                                     :function-type +fn-prototype+
                                     :argument-names +fn-prototype-argument-names+)
       (let* ((*run-all-function* ,run-all-fn)
              (*run-all-result* ,result)
              (*run-all-environment* ,fn-env-gs)
              (*irbuilder-run-all-alloca* ,irbuilder-alloca)
              (*irbuilder-run-all-body* ,irbuilder-body)
              (*current-function* ,run-all-fn)
              (*run-all-objects* nil))
         (cmp:with-dbg-function ("runAll-dummy-name"
                                 :linkage-name (llvm-sys:get-name ,run-all-fn)
                                 :function ,run-all-fn
                                 :function-type cmp:+fn-prototype+
                                 :form nil) ;; No form for run-all
           ;; Set up dummy debug info for these irbuilders
           (cmp:with-irbuilder (*irbuilder-run-all-alloca*)
             (cmp:dbg-set-current-source-pos nil))
           (cmp:with-irbuilder (*irbuilder-run-all-body*)
             (cmp:dbg-set-current-source-pos nil))
           (progn ,@body)
           (with-irbuilder (*irbuilder-run-all-body*)
             (let ((*gv-current-function-name* (jit-make-global-string-ptr
                                                (llvm-sys:get-name ,run-all-fn) "fn-name")))
               (with-landing-pad (irc-get-terminate-landing-pad-block ,fn-env-gs)
                 (irc-function-cleanup-and-return ,fn-env-gs ,result ))))))
       (values ,run-all-fn))))


(defmacro with-run-all-entry-codegen ((result) &body form)
  "Generate code within the ltv-function - used by codegen-load-time-value"
  `(let ((*irbuilder-function-alloca* *irbuilder-run-all-alloca*)
	 (*current-function* *run-all-function*)
         (,result *run-all-result*))
     (cmp:with-irbuilder (*irbuilder-run-all-alloca*)
       (cmp:with-landing-pad (irc-get-cleanup-landing-pad-block *run-all-environment*)
         ,@form))))

(defmacro with-run-all-body-codegen ((result) &body form)
  "Generate code within the ltv-function - used by codegen-load-time-value"
  `(let ((*irbuilder-function-alloca* *irbuilder-run-all-alloca*)
	 (*current-function* *run-all-function*)
         (,result *run-all-result*))
     (cmp:with-irbuilder (*irbuilder-run-all-body*)
       (cmp:with-landing-pad (irc-get-cleanup-landing-pad-block *run-all-environment*)
         ,@form))))

(defun ltv-global ()
  "called by cclasp"
  *load-time-value-holder-global-var*
  #+(or)(if cmp:*generate-compile-file-load-time-values*
      *load-time-value-holder-global-var*
      *run-time-values-table-global-var*))

(defun generate-load-time-values () *generate-compile-file-load-time-values*)

