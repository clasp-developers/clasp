(in-package :cmp)



(defvar *generate-compile-file-load-time-values* nil
  "This variable controls whether literals are compiled into the
load-time-value manager (true - in COMPILE-FILE) or not (false - in COMPILE)."
)



(defvar *load-time-value-result* nil
  "Temporary storage for results of evaluating top-level-forms")

;;; Contains the current RUN-ALL, initialization function
;;; for the current module
(defvar *load-time-value-initialization-function*)
(defvar +run-and-load-time-value-holder-global-var-type+ cmp:+ltv*+) ;; Was +ltvsp*+
(defvar *run-time-values-table-name* "run_time_values_table")
(defvar *load-time-initializer-environment*)
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


(defmacro with-ltv-function-codegen ((result env) &rest form)
  "Generate code within the ltv-function - used by codegen-load-time-value"
  `(let ((*irbuilder-function-alloca* *irbuilder-ltv-function-alloca*)
	 (*current-function* *load-time-value-initialization-function*)
	 (,result *load-time-value-result*)
	 (,env *load-time-initializer-environment*))
     (cmp:with-irbuilder (*irbuilder-ltv-function-body*)
       (cmp:with-landing-pad (irc-get-cleanup-landing-pad-block *load-time-initializer-environment*)
         ,@form))))

